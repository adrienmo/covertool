-record(config, {
    appname = 'Application',
    prefix_len = 0,
    cover_data = no_import,
    output = "coverage.xml",
    module_mapping = #{},
    summary = false,
    sources = ["src/"],
    beams = ["ebin/"]
}).
