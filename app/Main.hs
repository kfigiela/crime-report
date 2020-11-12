{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
     <> progDesc "List commits for IP report"
     <> header "crime reporter" )

listCommitsP :: Parser (IO ())
listCommitsP =
  listCommits
  <$> strArgument (metavar "OWNER")
  <*> strArgument (metavar "REPO")
  <*> strArgument (metavar "AUTHOR")
  <*> (flip UTCTime 0 <$> argument auto (metavar "FROM"))
  <*> (flip UTCTime 86400 <$> argument auto (metavar "TO"))


fetchCommitsSince :: Name Owner -> Name Repo -> UTCTime -> IO [(SimplePullRequest, [Commit])]
fetchCommitsSince owner repo from = do
  am <- getAuth
  Right prs <- executeRequestMaybe am $ pullRequestsForR owner repo (stateAll <> sortByUpdated <> sortDescending) (FetchAtLeast 300)
  putTextLn $ "Got " <> show (length prs) <> " pull requests."
  let recentlyUpdated = filter (\pr -> simplePullRequestUpdatedAt pr >= from) $ toList prs
  forM recentlyUpdated $ \pr -> do
    Right commits <- executeRequestMaybe am $ pullRequestCommitsR owner repo (simplePullRequestNumber pr) FetchAll
    putTextLn $ "Loaded commits for #" <> show (unIssueNumber $ simplePullRequestNumber pr) <> ": " <> simplePullRequestTitle pr
    pure (pr, toList commits)


listCommits :: Name Owner -> Name Repo -> Name User -> UTCTime -> UTCTime -> IO ()
listCommits owner repo author from to = do
  commits <- fetchCommitsSince owner repo from
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
