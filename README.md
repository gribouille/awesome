# awesome
`awesome` is a [awesome list](https://github.com/sindresorhus/awesome) generator.

`awesome`:

- get automatically for the Github repositories: stars, license, description, ...
- sorts the links by type and by stars
- generates the table of content.


Examples:
- [Python awesome list](https://github.com/gribouille/awesome-python)
- [Haskell awesome list](https://github.com/gribouille/awesome-haskell)

## Installation

### Prerequisites

Before installing `awesome`, you will need to make sure you have the `stack` tool
installed.

Archlinux:

```sh
$ sudo pacman -S stack
```

Ubuntu:

```sh
$ curl -sSL https://get.haskellstack.org/ | sh
```

Opensuse:

```sh
$ sudo zypper install stack
```

_Warning_: Opensuse Leap installs the version 1.5.0 which is buggy, see [documentation](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to install a most recent version.

### Downloading and Installing

The easiest way to install `awesome`, is to type:

```sh
$ git clone https://github.com/gribouille/awesome
$ cd awesome
$ stack install
$ awesome --version
```

## Configuration

`awesome` uses a JSON configuration file. The JSON structure
is a tree of `Category` objects. 

`Category` properties:

| Name        | Type       | Required | Description                     |
| ----------- | ---------- | -------- | ------------------------------- |
| `title`       | `string`     | yes      | Title of the category           |
| `description` | `string`     | no       | Description of the category     |
| `categories`  | `[Category]` | no       | Sub categories list.            |
| `items`       | `[Item]`     | no       | List of links in the categories |


`Item` properties:

| Name        | Type   | Required | Description                                                                                                                  |
| ----------- | ------ | -------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `url`         | `string` | yes      | Url of the item. For the Github repository, the url can be abbreviated to `<owner>/<repo>` for example `gribouille/awesome`. |
| `name`        | `string` | no       | Url name (automatically filled for Github repo).                                                                             |
| `description` | `string` | no       | Url description (automatically filled for Github repo).                                                                      |

There is no limitation of nested categories.

Example:

```json
{
  "title": "Python",
  "description": "...",
  "categories": [
    {
      "title": "Library",
      "categories": [
        {
          "title" : "Algorithms and Design Patterns",
          "description": "Python implementation of algorithms and design patterns.",
          "items": [
            {"url": "keon/algorithms"},
            {"url": "tylerlaberge/PyPattyrn"},
            {"url": "faif/python-patterns"},
            {"url": "http://www.grantjenks.com/docs/sortedcontainers/", "name": "sortedcontainers", "description": "Fast, pure-Python implementation of SortedList, SortedDict, and SortedSet types."}
          ]
        }
      ]
    },
    ...
  ]
}

```

## Usage

To generate an awesome list from a JSON file:

```
$ awesome config.json README.md
```

If you have Github repository in your list, you will be quickly limited to do
requests to the Github API without a [Github token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line).

To create a Github token, you need a Github account and go to: https://github.com/settings/tokens and click on _Generate new token_.

You can use this token with the option `--token`:

```
$ awesome config.json README.md --token <my_token>
```

## Contributing

Feedback and contributions are very welcome.

## License

This project is licensed under [Mozilla Public License Version 2.0](./LICENSE).