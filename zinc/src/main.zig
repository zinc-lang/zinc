const std = @import("std");
const Allocator = std.mem.Allocator;

const term = @import("term.zig");
const debug = @import("debug.zig");
const util = @import("util/util.zig");

const parse_glue = @import("parse/glue.zig");
const config = @import("config.zig");
const report = @import("report.zig");

const SourceMap = @import("SourceMap.zig");

// pub const log_level: std.log.Level = .debug;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        // // uncomment to print more useful information when debugging
        // // allocation related bugs, such as double-frees.
        // // Be warned: will use *a lot* more memory and be *much* slower.
        // .never_unmap = true,
        // .retain_metadata = true,
        // .stack_trace_frames = 8,
    }){
        // .backing_allocator = std.heap.c_allocator,
    };
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var timer = try util.Timer.start();

    const options = try Options.get(alloc);
    defer options.deinit(alloc);

    term.init();
    defer term.flush();

    const source = try util.readFile(alloc, options.file); // owned by the SourceMap

    var source_map = SourceMap{ .root = undefined };
    defer source_map.deinit(alloc);

    source_map.root = try source_map.files.append(alloc, SourceMap.SourceFile{
        .path = try std.fs.realpathAlloc(alloc, options.file),
        .src = source,
    });

    // const file = source_map.files.get(source_map.root);
    const file_ref = source_map.root;

    // Lex file
    timer.reset();
    var lexer_out = try parse_glue.Lexer.lexFile(alloc, source_map, file_ref);
    const time_lexer = timer.read();

    // Maybe dump tokens
    if (options.hasDump(.tokens)) {
        try debug.printTokens(
            std.fs.File.Writer,
            alloc,
            term.getStderr().writer(),
            // file.*,
            source_map.files.get(file_ref).*,
            true,
            true,
        );
        term.eprintln("", .{});
    }

    // Lexer reports
    report.printReports(source_map, lexer_out.strings, lexer_out.reps);
    report.exitIfErrors(lexer_out.reps);

    lexer_out.deinit(alloc);

    // Parse file
    timer.reset();
    var parser_out = parse_glue.Parser.parseFile(alloc, source_map, file_ref);
    const time_parser = timer.read();

    // Maybe dump cst
    if (options.hasDump(.cst)) {
        const file = source_map.files.get(file_ref);
        var w = debug.CstWriter(std.fs.File.Writer).init(
            alloc,
            term.getStderr().writer(),
            file.*,
            true,
            true,
        );
        try w.write(file.cst.rootIndex());
        term.eprintln("", .{});
    }

    // Parser reports
    report.printReports(source_map, parser_out.strings, parser_out.reps);
    report.exitIfErrors(parser_out.reps);

    parser_out.deinit(alloc);

    // Ast gen
    timer.reset();
    try parse_glue.Ast.Generator.generate(alloc, source_map, file_ref);
    const time_astgen = timer.read();

    if (options.times) {
        term.println("\ntimes: ", .{});
        term.println("\t lexer: \t{}", .{time_lexer});
        term.println("\t parser: \t{}", .{time_parser});
        term.println("\t astgen: \t{}", .{time_astgen});
    }
}

pub const Options = struct {
    file: []const u8,
    dumps: []const Dump,
    times: bool,

    pub const Dump = enum {
        tokens,
        cst,

        pub const Map = std.ComptimeStringMap(Dump, .{
            .{ "tokens", .tokens },
            .{ "cst", .cst },
        });
    };

    pub fn hasDump(self: Options, dump: Dump) bool {
        for (self.dumps) |d| {
            if (d == dump)
                return true;
        }
        return false;
    }

    pub fn deinit(self: Options, alloc: Allocator) void {
        alloc.free(self.file);
        alloc.free(self.dumps);
    }

    pub fn get(alloc: Allocator) !Options {
        const command = util.clap.Command{
            .name = "zinc",
            .version = config.version,
            // .short_help = config.description,
            .long_help = config.description,

            .args = &.{
                .{
                    .name = "FILE",
                    // .required = true,
                    .takes_value = true,
                    // .value_hint = .file_path,
                },
                .{
                    .name = "help",
                    .short = 'h',
                    .long = "help",
                    .short_help = "Display this message.",
                },
                .{
                    .name = "version",
                    .long = "version",
                    .short_help = "Print out version.",
                },
                .{
                    .name = "print_times",
                    .short = 'T',
                    .short_help = "Print how long things took.",
                },
                .{
                    .name = "dump",
                    .long = "dump",
                    .short = 'D',
                    .takes_value = true,
                    // .append = true,
                },
                .{
                    .name = "color",
                    .long = "color",
                    .takes_value = true,
                    .value_name = "WHEN",
                    .short_help = "[auto, never, always]",
                },
            },

            // .sub_commands = &.{
            //     .{
            //         .name = "test",
            //     },
            // },
        };

        const argv = try std.process.argsAlloc(alloc);
        defer std.process.argsFree(alloc, argv);

        var matches = try command.getMatches(alloc, argv);
        defer matches.deinit(alloc);

        if (matches.has("help")) {
            const help = try command.generateLongHelp(alloc);
            defer alloc.free(help);
            term.eprintln("{s}", .{help});
            std.os.exit(0);
        }

        if (matches.has("version")) {
            term.eprintln("{}", .{command.version.?});
            std.os.exit(0);
        }

        const file = if (matches.valueOf("FILE")) |file|
            try alloc.dupe(u8, file)
        else {
            term.eprint("{}error{s}: ", .{ term.Attr{ .color = .red }, term.Reset });
            term.eprintln("expected argument for FILE", .{});
            std.process.exit(2);
        };

        var dumps = std.ArrayList(Dump).init(alloc);
        for (matches.matches) |it| {
            if (it.is("dump")) {
                const dump = if (Dump.Map.get(it.value.?)) |d|
                    d
                else
                    util.todo("error", .{});
                try dumps.append(dump);
            }
        }

        const times = matches.has("print_times");

        return Options{
            .file = file,
            .dumps = dumps.toOwnedSlice(),
            .times = times,
        };
    }
};
