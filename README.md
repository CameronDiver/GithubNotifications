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

Then simply call the binary:

```
$ github-notifications
```

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

This will place the binary into `~/.local/bin` (at least on
my system).

## TODO

- Cache the calls to `getEnv` and `lookupEnv`
- Pass the etag back to Github to use cached values (will
  require caching notifications locally)
- Support pagination (maybe use a github library)
