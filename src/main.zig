//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

// const Board = @import("board");
const Game = @import("game");
var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const stderr = std.io.getStdErr().writer();
    const allocator = debug_allocator.allocator();
    var game = Game.init(.Classic);
    game.mainLoop(allocator) catch |err| switch (err) {
        error.EndGameError => {
            try stderr.print("\n\nCheckmate! {s} won!\nGGs :)\n", .{if (game.currentSide == .Black) "White" else "Black"});
        },
        else => {
            try stderr.print("game ended with an unknown reason: {!}\n", .{err});
        },
    };
    // std.debug.print("{any}\n", .{move});
    defer _ = debug_allocator.deinit();
}

const std = @import("std");
