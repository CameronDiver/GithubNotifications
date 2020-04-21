# GithubNotifications

Display your github notifications on the command line.
Mainly a project to get some experience with Haskell and
Aeson + web requests.

## Usage

Add an auth.json file to
`$XDG_CONFIG_HOME/GithubNotifications` with your username
and personal access token:

```json
{
	"username": "CameronDiver",
	"token": "*****************"
}
```

Note that the personal access token requires the scopes
notifications and repos.

Start the notification daemon which will periodically check
for notifications:

```
github-notifications-daemon &
```

Then simply call the binary:

```
$ github-notifications
```

### Why a daemon?

I made this to add the end of my `~/.zshrc` to display my
github notifications with every new shell. The lookup from
API PR URL to browser URL is too long for my purposes, so
having it cached in memory and requested from the daemon is
much quicker.

## Installing

> Requires: stack

- Clone this repo:

```
$ git clone https://github.com/CameronDiver/GithubNotifications && cd GithubNotification
```

- Build:

```
$ stack build
```

- Install:

```
$ stack install
```

This will place the binaries into `~/.local/bin` (at least on
my system).

## TODO

- Cache the calls to `getEnv` and `lookupEnv`
- Pass the etag back to Github to use cached values
- Support pagination (maybe use a github library)
