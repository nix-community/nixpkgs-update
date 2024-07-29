{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Version
  ( assertCompatibleWithPathPin,
    matchVersion,
  )
where

import Data.Char (isAlpha, isDigit)
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.PartialOrd as PO
import qualified Data.Text as T
import Data.Versions (SemVer (..), VUnit (..), semver)
import OurPrelude
import Utils

notElemOf :: (Eq a, Foldable t) => t a -> a -> Bool
notElemOf o = not . flip elem o

-- | Similar to @breakOn@, but will not keep the pattern at the beginning of the suffix.
--
-- Examples:
--
-- >>> clearBreakOn "::" "a::b::c"
-- ("a","b::c")
clearBreakOn :: Text -> Text -> (Text, Text)
clearBreakOn boundary string =
  let (prefix, suffix) = T.breakOn boundary string
   in if T.null suffix
        then (prefix, suffix)
        else (prefix, T.drop (T.length boundary) suffix)

-- | Check if attribute path is not pinned to a certain version.
-- If a derivation is expected to stay at certain version branch,
-- it will usually have the branch as a part of the attribute path.
--
-- Examples:
--
-- >>> versionCompatibleWithPathPin "libgit2_0_25" "0.25.3"
-- True
--
-- >>> versionCompatibleWithPathPin "owncloud90" "9.0.3"
-- True
--
-- >>> versionCompatibleWithPathPin "owncloud-client" "2.4.1"
-- True
--
-- >>> versionCompatibleWithPathPin "owncloud90" "9.1.3"
-- False
--
-- >>> versionCompatibleWithPathPin "nodejs-slim-10_x" "11.2.0"
-- False
--
-- >>> versionCompatibleWithPathPin "nodejs-slim-10_x" "10.12.0"
-- True
--
-- >>> versionCompatibleWithPathPin "firefox-esr-78-unwrapped" "91.1.0esr"
-- False
versionCompatibleWithPathPin :: Text -> Version -> Bool
versionCompatibleWithPathPin attrPath newVer
  | "-unwrapped" `T.isSuffixOf` attrPath =
      versionCompatibleWithPathPin (T.dropEnd 10 attrPath) newVer
  | "_x" `T.isSuffixOf` T.toLower attrPath =
      versionCompatibleWithPathPin (T.dropEnd 2 attrPath) newVer
  | "_" `T.isInfixOf` attrPath =
      let attrVersionPart =
            let (_, version) = clearBreakOn "_" attrPath
             in if T.any (notElemOf ('_' : ['0' .. '9'])) version
                  then Nothing
                  else Just version
          -- Check assuming version part has underscore separators
          attrVersionPeriods = T.replace "_" "." <$> attrVersionPart
       in -- If we don't find version numbers in the attr path, exit success.
          maybe True (`T.isPrefixOf` newVer) attrVersionPeriods
  | otherwise =
      let attrVersionPart =
            let version = T.dropWhile (notElemOf ['0' .. '9']) attrPath
             in if T.any (notElemOf ['0' .. '9']) version
                  then Nothing
                  else Just version
          -- Check assuming version part is the prefix of the version with dots
          -- removed. For example, 91 => "9.1"
          noPeriodNewVersion = T.replace "." "" newVer
       in -- If we don't find version numbers in the attr path, exit success.
          maybe True (`T.isPrefixOf` noPeriodNewVersion) attrVersionPart

versionIncompatibleWithPathPin :: Text -> Version -> Bool
versionIncompatibleWithPathPin path version =
  not (versionCompatibleWithPathPin path version)

assertCompatibleWithPathPin :: (Monad m) => UpdateEnv -> Text -> ExceptT Text m ()
assertCompatibleWithPathPin ue attrPath =
  tryAssert
    ( "Version in attr path "
        <> attrPath
        <> " not compatible with "
        <> newVersion ue
    )
    ( not
        ( versionCompatibleWithPathPin attrPath (oldVersion ue)
            && versionIncompatibleWithPathPin attrPath (newVersion ue)
        )
    )

data VersionPart
  = PreReleasePart VersionPart
  | EmptyPart
  | IntPart Word
  | TextPart Text
  deriving (Show, Eq)

data ParsedVersion
  = SemanticVersion SemVer
  | SimpleVersion [VersionPart]
  deriving (Show, Eq)

preReleaseTexts :: [Text]
preReleaseTexts = ["alpha", "beta", "pre", "rc"]

textPart :: Text -> VersionPart
textPart t
  | tLower `elem` preReleaseTexts = PreReleasePart $ TextPart tLower
  | otherwise = TextPart tLower
  where
    tLower = T.toLower t

class SimpleVersion a where
  simpleVersion :: a -> [VersionPart]

instance SimpleVersion Text where
  simpleVersion t
    | digitHead /= "" = IntPart number : simpleVersion digitTail
    | alphaHead /= "" = textPart alphaHead : simpleVersion alphaTail
    | otherwise = []
    where
      t' = T.dropWhile (\c -> not (isAlpha c || isDigit c)) t
      (digitHead, digitTail) = T.span isDigit t'
      number = read $ T.unpack digitHead
      (alphaHead, alphaTail) = T.span isAlpha t'

instance SimpleVersion ParsedVersion where
  simpleVersion (SimpleVersion v) = v
  simpleVersion (SemanticVersion v) = simpleVersion v

instance SimpleVersion SemVer where
  simpleVersion SemVer {_svMajor, _svMinor, _svPatch, _svPreRel} =
    [IntPart _svMajor, IntPart _svMinor, IntPart _svPatch]
      ++ map toPart (concat (fmap toList _svPreRel))
    where
      toPart :: VUnit -> VersionPart
      toPart (Digits i) = IntPart i
      toPart (Str t) =
        case textPart t of
          PreReleasePart p -> PreReleasePart p
          p -> PreReleasePart p

instance SimpleVersion [VersionPart] where
  simpleVersion = id

-- | Pre-release parts come before empty parts, everything else comes after
-- them. Int and text parts compare to themselves as expected and comparison
-- between them is not defined.
instance PO.PartialOrd VersionPart where
  PreReleasePart a <= PreReleasePart b = a PO.<= b
  PreReleasePart _ <= _ = True
  _ <= PreReleasePart _ = False
  EmptyPart <= _ = True
  _ <= EmptyPart = False
  IntPart a <= IntPart b = a <= b
  TextPart a <= TextPart b = a <= b
  _ <= _ = False

-- | If either version contains no comparable parts, the versions are not
-- comparable. If both contain at least some parts, compare parts in order. When
-- a version runs out of parts, its remaining parts are considered empty parts,
-- which come after pre-release parts, but before other parts.
--
-- Examples:
--
-- >>> on PO.compare parseVersion "1.2.3" "1.2.4"
-- Just LT
--
-- >>> on PO.compare parseVersion "1.0" "-"
-- Nothing
--
-- >>> on PO.compare parseVersion "-" "-"
-- Nothing
--
-- >>> on PO.compare parseVersion "1.0" "1_0_0"
-- Just LT
--
-- >>> on PO.compare parseVersion "1.0-pre3" "1.0"
-- Just LT
--
-- >>> on PO.compare parseVersion "1.1" "1.a"
-- Nothing
instance PO.PartialOrd ParsedVersion where
  SemanticVersion a <= SemanticVersion b = a <= b
  SimpleVersion [] <= _ = False
  _ <= SimpleVersion [] = False
  a <= b = on lessOrEq simpleVersion a b
    where
      lessOrEq [] [] = True
      lessOrEq [] ys = lessOrEq [EmptyPart] ys
      lessOrEq xs [] = lessOrEq xs [EmptyPart]
      lessOrEq (x : xs) (y : ys) =
        case PO.compare x y of
          Just LT -> True
          Just EQ -> lessOrEq xs ys
          Just GT -> False
          Nothing -> False

parseVersion :: Version -> ParsedVersion
parseVersion v =
  case semver v of
    Left _ -> SimpleVersion $ simpleVersion v
    Right v' -> SemanticVersion v'

matchUpperBound :: Boundary Version -> Version -> Bool
matchUpperBound Unbounded _ = True
matchUpperBound (Including b) v = parseVersion v PO.<= parseVersion b
matchUpperBound (Excluding b) v = parseVersion v PO.< parseVersion b

matchLowerBound :: Boundary Version -> Version -> Bool
matchLowerBound Unbounded _ = True
matchLowerBound (Including b) v = parseVersion b PO.<= parseVersion v
matchLowerBound (Excluding b) v = parseVersion b PO.< parseVersion v

-- | Reports True only if matcher certainly matches. When the order or equality
-- of versions is ambiguous, return False.
--
-- Examples:
--
-- >>> matchVersion (SingleMatcher "1.2.3") "1_2-3"
-- True
--
-- >>> matchVersion (RangeMatcher Unbounded (Including "1.0-pre3")) "1.0"
-- False
--
-- >>> matchVersion (RangeMatcher Unbounded (Excluding "1.0-rev3")) "1.0"
-- True
matchVersion :: VersionMatcher -> Version -> Bool
matchVersion (SingleMatcher v) v' = parseVersion v PO.== parseVersion v'
matchVersion (RangeMatcher lowerBound upperBound) v =
  matchLowerBound lowerBound v && matchUpperBound upperBound v
