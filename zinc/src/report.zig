const std = @import("std");
const Allocator = std.mem.Allocator;

const parse_glue = @import("parse/glue.zig");
const util = @import("util/util.zig");

const term = @import("term.zig");

const SourceMap = @import("SourceMap.zig");

pub const Report = struct {
    /// Severity of the message
    level: Level,
    /// Main description, should stand out on its own
    message: util.Strings.Ref,
    /// The primary piece of code in question
    primary_range: parse_glue.Token.SourceLocalRange,

    file_ref: SourceMap.SourceFiles.Ref,

    // /// A short message, that can still make sense given where it points to
    // primary_message: ?[]const u8 = null,

    // secondary_range: ?parse_glue.Token.SourceLocalRange = null,
    // secondary_message: ?[]const u8 = null,

    // suggestion: ? an optional way to show how and what code to modify to fix the issue

    pub const Level = enum {
        /// Possible problem, but not dire
        warning,
        /// A problem that needs attention to be fixed
        error_,
        // /// Suggest an action
        // help,
        // /// Provide context
        // note,
        /// Something that has not been implemented yet, with no guarantee that it will be
        unimpl,
    };
};

pub fn printReport(source_map: SourceMap, strings: util.Strings, rep: Report) void {
    const file = source_map.files.get(rep.file_ref);
    const range = rep.primary_range;

    // Write level
    switch (rep.level) {
        .warning => term.eprint("{}warning{s}: ", .{ term.Attr{ .color = .yellow }, term.Reset }),
        .error_ => term.eprint("{}error{s}: ", .{ term.Attr{ .color = .red }, term.Reset }),
        .unimpl => term.eprint("{}unimpl{s}: ", .{ term.Attr{ .color = .cyan }, term.Reset }),
    }

    // Write message
    term.eprintln("{s}", .{strings.get(rep.message)});
    // term.eprintln("", .{});

    const start_line_index = file.getLineOffsetIndex(range.start);
    const end_line_index = file.getLineOffsetIndex(range.end);

    const start_column = range.start - file.line_offsets[start_line_index];

    // Get what the length of the largest line number would be as a string
    const padding_len = std.math.log10(std.math.max(start_line_index, end_line_index)) + 2;

    // Write out the file, line and column
    term.eprint(" {s:[1]}", .{ "@", padding_len });
    term.eprintln(": {s}:{}:{}", .{ file.path, start_line_index + 1, start_column });

    // // Blank line before source code printout
    // term.eprintln(" {s:[1]}", .{ "|", padding_len });

    // Write the code view
    if (start_line_index == end_line_index) {
        const line_number = start_line_index + 1;

        // Write out the line number
        term.eprint("{} | ", .{line_number});

        const line_slice = file.getLineSlice(start_line_index);

        const start_line_offset = file.line_offsets[start_line_index];
        for (line_slice) |ch, i| {
            // Get the current index/offset within the source code
            const index = start_line_offset + i + 1;
            // If our character is in the range, highlight it
            if (index >= range.start + 1 and index <= range.end) {
                term.eprint("{}", .{term.Attr{ .color = .red }});
                term.eprint("{c}{s}", .{ ch, term.Reset });
            } else {
                term.eprint("{c}", .{ch});
            }
        }

        term.eprintln("", .{});
    } else {
        // util.todo("multiple line code view", .{});
        term.eprintln("TODO: multiple line code view", .{});
    }

    // // Blank line after source code printout
    // term.eprintln(" {s:[1]}", .{ "|", padding_len });
}

pub fn printReports(source_map: SourceMap, strings: util.Strings, reports: []const Report) void {
    printReportsForLevel(source_map, strings, reports, .warning);
    printReportsForLevel(source_map, strings, reports, .error_);
    printReportsForLevel(source_map, strings, reports, .unimpl);
}

pub fn printReportsForLevel(
    source_map: SourceMap,
    strings: util.Strings,
    reports: []const Report,
    level: Report.Level,
) void {
    for (reports) |rep| {
        if (rep.level != level)
            continue;

        printReport(source_map, strings, rep);

        term.eprintln("", .{});
    }
}

pub fn exitIfErrors(reports: []const Report) void {
    for (reports) |rep| {
        if (rep.level == .error_) {
            term.eprintln("Exiting due to error(s).", .{});
            std.process.exit(0);
        }
    }
}
