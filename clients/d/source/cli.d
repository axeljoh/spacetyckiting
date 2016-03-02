import std.getopt;

struct GameConfig
{
    string host = "localhost";
    ushort port = 3000;
    string name = "Awesome D bot";
    bool verbose;
}

GameConfig parseCLI(string[] args)
{
    GameConfig gconfig;
    auto helpInformation = getopt(
    args,
    config.passThrough,
    "host|H",  &gconfig.host,    // numeric
    "port|P",    &gconfig.port,      // string
    "verbose|v", &gconfig.verbose,   // flag
    "name|n", &gconfig.name,   // flag
    );
    if (helpInformation.helpWanted || args.length > 1)
    {
        import std.c.stdlib;
        defaultGetoptPrinter("D Tyckiting client ",
        helpInformation.options);
        exit(0);
    }
    return gconfig;
}
