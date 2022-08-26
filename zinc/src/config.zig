const std = @import("std");

pub const version = std.SemanticVersion.parse("0.0.1") catch unreachable;
pub const description = "The zinc programming language.";
