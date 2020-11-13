{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Universum

import System.Environment ( lookupEnv )
import GitHub
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime(..))
import Options.Applicative

main :: IO ()
main = join $ execParser opts
  where
    opts = info (listCommitsP <**> helper)
      ( fullDesc
     <> progDesc "Make sure to set GITHUB_TOKEN to access private repos."
     <> header "crime reporter – List contributions for copyright transfer report. "
      )
listCommitsP :: Parser (IO ())
listCommitsP =
  listCommits
  <$> strArgument (metavar "OWNER")
  <*> strArgument (metavar "REPO")
  <*> strArgument (metavar "AUTHOR")
  <*> (flip UTCTime 0 <$> argument auto (metavar "FROM"))
  <*> (flip UTCTime 86400 <$> argument auto (metavar "TO"))
  <*> option auto (long "maxPulls" <> metavar "COUNT" <> help "Number of recent pull-requests to check" <> value 300)

type MaxPulls = Word

fetchCommitsSince :: Name Owner -> Name Repo -> UTCTime -> MaxPulls -> IO [(SimplePullRequest, [Commit])]
fetchCommitsSince owner repo from maxPulls = do
  am <- getAuth
  prs <- throwLeft $ executeRequestMaybe am $ pullRequestsForR owner repo (stateAll <> sortByUpdated <> sortDescending) (FetchAtLeast maxPulls)
  let recentlyUpdated = filter (\pr -> simplePullRequestUpdatedAt pr >= from) $ toList prs
  log $ "$ Got " <> show (length prs) <> " pull requests, only " <> show (length recentlyUpdated) <> " updated since " <> show from
  forM (recentlyUpdated `zip` [1..]) $ \(pr, ix) -> do
    commits <- throwLeft $ executeRequestMaybe am $ pullRequestCommitsR owner repo (simplePullRequestNumber pr) FetchAll
    log $ "$ " <> show ix <> "/" <> show (length recentlyUpdated) <> ": #" <> show (unIssueNumber $ simplePullRequestNumber pr) <> ": " <> simplePullRequestTitle pr
    pure (pr, toList commits)

listCommits :: Name Owner -> Name Repo -> Name User -> UTCTime -> UTCTime -> MaxPulls ->  IO ()
listCommits owner repo author from to maxPulls = do
  commits <- fetchCommitsSince owner repo from maxPulls
  let
    filteredPRs =
      commits
      & map (second $ filter (\c -> authoredBy author c && betweenDates from to c))
      & filter (not . null . snd)
  forM_ filteredPRs $ \(pr, commits) -> do
    let hashes = commitShortSha <$> commits
    putTextLn $ "#" <> show (unIssueNumber $ simplePullRequestNumber pr) <> ": " <> simplePullRequestTitle pr <> " (" <> Text.intercalate ", " hashes <> ")"

authoredBy :: Name User -> Commit -> Bool
authoredBy login commit = map simpleUserLogin (commitAuthor commit) == Just login

betweenDates :: UTCTime -> UTCTime -> Commit -> Bool
betweenDates from to commit = authorDate >= from && authorDate < to
  where
    authorDate = gitUserDate . gitCommitAuthor . commitGitCommit $ commit

getAuth :: IO (Maybe Auth)
getAuth = do
  token <- lookupEnv "GITHUB_TOKEN"
  pure (OAuth . fromString <$> token)

commitShortSha :: Commit -> Text
commitShortSha = Text.take 7 . untagName . commitSha

log :: Text -> IO ()
log = hPutStrLn stderr

throwLeft :: (MonadThrow m, Exception a) => m (Either a b) -> m b
throwLeft action = action >>= \case
  Right a -> pure a
  Left e -> throwM e
