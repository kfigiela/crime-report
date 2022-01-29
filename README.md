# crime-report – List contributions to GitHub repo for copyright transfer report

Generates a list of all contributions to pull requests a specific GitHub user to the given repo within a specified period of time. The list includes PR number, title and short commit hashes. We use this to automate paperwork related to copyright transfers to clients.

## Usage:

Firstly export your GITHUB_TOKEN if you want to access private repository

```export GITHUB_TOKEN=xyz```

For repo `https://github.com/owner/repo/`, the following will yield report for user with login `lordvader` for October 2025.

```stack run -- owner repo lordvader 2025-10-01 2025-10-31```
