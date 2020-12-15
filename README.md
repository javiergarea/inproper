inproper
=====

Induction of PropEr properties based on sample unit tests.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {inproper, {git, "https://host/user/inproper.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 inproper
    ===> Fetching inproper
    ===> Compiling inproper
    <Plugin Output>
