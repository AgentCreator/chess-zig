//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
// const testing = std.testing;

// const Type = std.builtin.Type;
//
// pub fn structCurry(comptime T: type, comptime defaults: anytype) type {
//     const old_info = @typeInfo(T).@"struct";
//     var new_fields: [old_info.fields.len]Type.StructField = old_info.fields[0..].*;
//     for (&new_fields) |*f| {
//         if (@hasField(@TypeOf(defaults), f.name)) {
//             const default_val: f.type = @field(defaults, f.name);
//             f.default_value_ptr = @ptrCast(&default_val);
//         }
//     }
//     return @Type(.{ .@"struct" = .{
//         .backing_integer = old_info.backing_integer,
//         .decls = &.{},
//         .fields = &new_fields,
//         .is_tuple = old_info.is_tuple,
//         .layout = old_info.layout,
//     } });
// }
