# kanin

[![Build Status][travis badge]][travis]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]
[![Downloads][hex downloads]][hex package]

[![][project-logo]][project-logo-source]

*An LFE Wrapper for the Erlang RabbitMQ (AMQP) Client*


## Table of Contents

* [Introduction](#introduction-)
* [Installation](#installation-)
* [Documentation](#documentation-)
* [Usage](#usage-)
* [License](#license-)


## Introduction [&#x219F;](#table-of-contents)

The purose of the kanin library is twofold:

1. To provide a sensible LFE wrapper for various modules in the Erlang AMQP
   client libraries, namely
   [rabbitmq-erlang-client](https://github.com/rabbitmq/rabbitmq-erlang-client)
   and
   [rabbitmq-common](https://github.com/rabbitmq/rabbitmq-common); and
2. To provide a high-level API that protects the low-level API with a
   `gen_server` and an OTP supervision tree with a configurable restart
   strategy to safeguard against failure conditions.

While initially kanin was created to provide a less verbose AMQP client
library for LFE hackers (and one that was also more Lispy), it has begun
evolving to provide additional, more robust features useful in real-world
applications and deployment conditions.


## Installation [&#x219F;](#table-of-contents)

To pull in kanin as part of your project, just add it to your `rebar.config`
deps:

```erlang
  {deps, [
    ...
    {kanin, {git, "git@github.com:lfex/kanin.git", {branch, "master"}}}
  ]}.
```

And then do:

```bash
    $ rebar3 compile
```


## Documentation [&#x219F;](#table-of-contents)

Kanin offers two levels of documentation:

 * [LFE Wrapper Docs](docs/wrapper/index.md) - no-frills,
   LFE-ized version of the RabbitMQ Erlang libs
 * [Kanin OTP Docs](docs/otp/index.md) - a version of the API with
   a supervision tree for those with more robust needs

Also, you may be interested in the
[Kanin tutorials](http://billo.gitbooks.io/lfe-rabbitmq-tutorials/),
which have been translated into LFE from the official RabbitMQ docs for Python
and Erlang.


## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2014-2017, BilloSystems, Ltd. Co.

Copyright © 2015-2017, Ricardo Lanziano

Copyright © 2015-2017, Duncan McGreggor


<!-- Named page links below: /-->

[project-logo]: resources/images/kanin-small.png
[project-logo-large]: resources/images/kanin.png
[project-logo-source]: http://aquarius-galuxy.deviantart.com/art/Rabbit-Drawing-176749973
[org]: https://github.com/lfex
[github]: https://github.com/lfex/kanin
[gitlab]: https://gitlab.com/lfex/kanin
[travis]: https://travis-ci.org/lfex/kanin
[travis badge]: https://img.shields.io/travis/lfex/kanin.svg
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.2+-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-17+-blue.svg
[versions]: https://github.com/lfex/kanin/blob/master/.travis.yml
[github tags]: https://github.com/lfex/kanin/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/kanin.svg
[github downloads]: https://img.shields.io/github/downloads/atom/atom/total.svg
[hex badge]: https://img.shields.io/hexpm/v/kanin.svg
[hex package]: https://hex.pm/packages/kanin
[hex downloads]: https://img.shields.io/hexpm/dt/kanin.svg
