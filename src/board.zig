const assert = @import("std").debug.assert;
const testing = @import("std").testing;

const Self = @This();

const std = @import("std");

const Piece = enum {
    empty,
    Bking,
    Bqueen,
    Brook,
    Bbishop,
    Bknight,
    Bpawn,
    Wking,
    Wqueen,
    Wrook,
    Wbishop,
    Wknight,
    Wpawn,

    const Color = enum(u1) {
        White,
        Black,
    };

    fn color(self: Piece) ?Color {
        if (self == .empty) return null;
        return switch (self) {
            .Wking, .Wqueen, .Wrook, .Wbishop, .Wknight, .Wpawn => .White,
            else => .Black,
        };
    }
};

inline fn intCast(comptime T: type, int: anytype) T {
    return @as(T, @intCast(int));
}

fn indexOf(comptime T: type, haystack: []const T, needle: T) error{NoNeedleFound}!usize {
    return blk: for (haystack, 0..) |e, idx| {
        if (e == needle) break :blk idx;
    } else error.NoNeedleFound;
}

test "indexOf tests success" {
    // String tests
    try testing.expectEqual(@as(usize, 0), try indexOf(u8, "hello", 'h'));
    try testing.expectEqual(@as(usize, 4), try indexOf(u8, "hello", 'o'));
    try testing.expectEqual(@as(usize, 2), try indexOf(u8, "hello", 'l'));

    // Integer array tests
    try testing.expectEqual(@as(usize, 0), try indexOf(i32, &[_]i32{ 1, 2, 3 }, 1));
    try testing.expectEqual(@as(usize, 2), try indexOf(i32, &[_]i32{ 1, 2, 3 }, 3));

    // Boolean array test
    try testing.expectEqual(@as(usize, 1), try indexOf(bool, &[_]bool{ false, true, false }, true));

    // Error cases
    try testing.expectError(error.NoNeedleFound, indexOf(u8, "hello", 'x'));
    try testing.expectError(error.NoNeedleFound, indexOf(i32, &[_]i32{ 1, 2, 3 }, 4));
}

test "indexOf tests error" {
    // Empty array tests
    try testing.expectError(error.NoNeedleFound, indexOf(u8, "", 'a'));
    try testing.expectError(error.NoNeedleFound, indexOf(i32, &[_]i32{}, 1));

    // Not found tests
    try testing.expectError(error.NoNeedleFound, indexOf(u8, "abc", 'd'));
    try testing.expectError(error.NoNeedleFound, indexOf(i32, &[_]i32{ 1, 2, 3 }, 5));
    try testing.expectError(error.NoNeedleFound, indexOf(bool, &[_]bool{ true, true }, false));
}

fn squereToInt(squere: []const u8) error{ NoNeedleFound, WrongLength }!u6 {
    if (squere.len != 2) return error.WrongLength;
    const cols = "abcdefgh";
    const rows = "87654321";
    return 8 * intCast(u6, try indexOf(u8, rows, squere[1])) + intCast(u6, try indexOf(u8, cols, squere[0]));
}

test "squereToInt" {
    try testing.expect(try squereToInt("a8") == 0);
    try testing.expect(try squereToInt("h8") == 7);
    try testing.expect(try squereToInt("h1") == 63);
    try testing.expect(try squereToInt("a1") == 56);
    try testing.expectError(error.NoNeedleFound, squereToInt("i8"));
    try testing.expectError(error.NoNeedleFound, squereToInt("g9"));
    try testing.expectError(error.NoNeedleFound, squereToInt("q_"));
    try testing.expectError(error.NoNeedleFound, squereToInt("A2"));
    try testing.expectError(error.WrongLength, squereToInt("i"));
    try testing.expectError(error.WrongLength, squereToInt(""));
    try testing.expectError(error.WrongLength, squereToInt("a really really really really really wrong and terrible way to say \"e4\" "));
    try testing.expectError(error.WrongLength, squereToInt("fun fact: unit tests are annoying"));
}

pub const Move = packed struct(u12) {
    from: u6,
    to: u6,

    pub fn fromNotation(notation: []const u8, color: Piece.Color) error{ NotAParsableMove, NoNeedleFound, WrongLength }!Move {
        var splitNotation = std.mem.splitAny(u8, notation, " -xX");
        const from, const to = .{ splitNotation.first(), splitNotation.rest() };
        if (from.len == 1 and from[0] == 'O') {
            //castles
            return Move{
                .from = switch (color) {
                    .White => comptime squereToInt("e1") catch unreachable,
                    .Black => comptime squereToInt("e8") catch unreachable,
                },
                .to = if (to.len == 1) switch (color) {
                    .White => comptime squereToInt("g1") catch unreachable,
                    .Black => comptime squereToInt("g8") catch unreachable,
                } else if (to.len == 3) switch (color) {
                    .White => comptime squereToInt("c1") catch unreachable,
                    .Black => comptime squereToInt("c8") catch unreachable,
                } else unreachable,
            };
        }
        // goofy ahh solution
        if (from.len == 3) return fromNotation(notation[1..], color);

        if (from.len != 2 or to.len != 2) {
            return error.NotAParsableMove;
        }

        return Move{ .from = try squereToInt(from), .to = try squereToInt(to) };
    }

    fn toInt(self: Move) u12 {
        return @bitCast(self);
    }
};
test "move from notation" {
    // Regular moves with different separators
    try testing.expectEqual(
        Move{ .from = try squereToInt("e2"), .to = try squereToInt("e4") },
        try Move.fromNotation("e2-e4", .White),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("d7"), .to = try squereToInt("d5") },
        try Move.fromNotation("d7 d5", .Black),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("f6"), .to = try squereToInt("e4") },
        try Move.fromNotation("f6xe4", .Black),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("g1"), .to = try squereToInt("f3") },
        try Move.fromNotation("g1Xf3", .White),
    );

    // Castling moves
    try testing.expectEqual(
        Move{ .from = try squereToInt("e1"), .to = try squereToInt("g1") },
        try Move.fromNotation("O-O", .White),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("e8"), .to = try squereToInt("g8") },
        try Move.fromNotation("O-O", .Black),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("e1"), .to = try squereToInt("c1") },
        try Move.fromNotation("O-O-O", .White),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("e8"), .to = try squereToInt("c8") },
        try Move.fromNotation("O-O-O", .Black),
    );

    // Error cases
    try testing.expectError(error.NotAParsableMove, Move.fromNotation("", .White));
    try testing.expectError(error.NotAParsableMove, Move.fromNotation("invalid", .Black));
    try testing.expectError(error.NotAParsableMove, Move.fromNotation("e4-654e", .White));
    try testing.expectError(error.NoNeedleFound, Move.fromNotation("i9-e4", .Black));
    try testing.expectError(error.NoNeedleFound, Move.fromNotation("h9-e4", .White));
    try testing.expectError(error.NoNeedleFound, Move.fromNotation("a0-e4", .Black));
}

// BOARD

board: [64]Piece = undefined,
moveHistory: [9e3]?Move = @splat(null),
// 4096 = 2^12
// intented usage: legalMoves[move.toInt()]
legalMoves: [4096]bool = undefined,

pub fn fromFen(comptime fen: []const u8) Self {
    var board: Self = Self{};

    comptime var i: usize = 0;

    inline for (fen) |chr| {
        const piece: Piece = switch (chr) {
            'r' => .Brook,
            'n' => .Bknight,
            'b' => .Bbishop,
            'q' => .Bqueen,
            'k' => .Bking,
            'p' => .Bpawn,
            'R' => .Wrook,
            'N' => .Wknight,
            'B' => .Wbishop,
            'Q' => .Wqueen,
            'K' => .Wking,
            'P' => .Wpawn,
            '1'...'8' => |n| {
                i += comptime std.fmt.parseInt(u6, &[1]u8{n}, 10) catch unreachable;
                continue;
            },
            '/' => continue,
            else => unreachable,
        };
        board.board[i] = piece;
        i +%= 1;
    }
    return board;
}

pub fn prettyPrint(self: Self) void {
    for (self.board, 0..) |pieceType, i| {
        const piece: u21 = switch (pieceType) {
            .empty => '.',
            .Wpawn => '♙',
            .Bpawn => '♟',
            .Wknight => '♘',
            .Bknight => '♞',
            .Wbishop => '♗',
            .Bbishop => '♝',
            .Wrook => '♖',
            .Brook => '♜',
            .Wqueen => '♕',
            .Bqueen => '♛',
            .Wking => '♔',
            .Bking => '♚',
        };
        const chr: u8 = if ((i + 1) % 8 == 0) '\n' else ' ';
        std.debug.print("{u}{c}", .{ piece, chr });
    }
}

test "fromFen" {
    const startingPosition = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");
    try testing.expectEqual(Piece.Brook, startingPosition.board[0]);
    try testing.expectEqual(Piece.Bknight, startingPosition.board[1]);
    try testing.expectEqual(Piece.Wking, startingPosition.board[60]);
    try testing.expectEqual(Piece.Wrook, startingPosition.board[63]);

    const emptyBoard = fromFen("8/8/8/8/8/8/8/8");
    try testing.expectEqual(Piece.empty, emptyBoard.board[0]);
    try testing.expectEqual(Piece.empty, emptyBoard.board[63]);
}

fn forceMakeMove(self: *Self, move: Move) void {
    const piece = self.*.board[move.from];
    self.board[move.from] = .empty;
    self.board[move.to] = piece;
}

test "force make move test" {
    var board = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");

    // Test basic pawn move
    board.forceMakeMove(Move{ .from = try squereToInt("e2"), .to = try squereToInt("e4") });
    try testing.expectEqual(Piece.empty, board.board[try squereToInt("e2")]);
    try testing.expectEqual(Piece.Wpawn, board.board[try squereToInt("e4")]);

    // Test piece capture
    board = fromFen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR");
    board.forceMakeMove(Move{ .from = try squereToInt("e4"), .to = try squereToInt("d5") });
    try testing.expectEqual(Piece.empty, board.board[try squereToInt("e4")]);
    try testing.expectEqual(Piece.Wpawn, board.board[try squereToInt("d5")]);

    // Test knight move
    board = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");
    board.forceMakeMove(Move{ .from = try squereToInt("g1"), .to = try squereToInt("f3") });
    try testing.expectEqual(Piece.empty, board.board[try squereToInt("g1")]);
    try testing.expectEqual(Piece.Wknight, board.board[try squereToInt("f3")]);
}

fn addLegalMove(self: *Self, move: Move) void {
    self.legalMoves[move.toInt()] = true;
}

fn getXYfromPos(pos: u6) [2]i8 {
    return .{ @mod(pos, 8), @divFloor(pos, 8) };
}

test "getXYfromPos" {
    try testing.expectEqual([2]i8{ 0, 0 }, getXYfromPos(0)); // a8
    try testing.expectEqual([2]i8{ 7, 0 }, getXYfromPos(7)); // h8
    try testing.expectEqual([2]i8{ 0, 7 }, getXYfromPos(56)); // a1
    try testing.expectEqual([2]i8{ 7, 7 }, getXYfromPos(63)); // h1
    try testing.expectEqual([2]i8{ 4, 3 }, getXYfromPos(28)); // e5
    try testing.expectEqual([2]i8{ 2, 5 }, getXYfromPos(42)); // c3
}

fn isOnBoard(x: i8, y: i8) bool {
    return x >= 0 and x < 8 and y >= 0 and y < 8;
}

fn addDirections(self: *Self, start: u6, comptime directions: []const [2]i8) void {
    const color = self.board[start].color().?;
    for (directions) |direction| {
        std.debug.print("direction: {any}\n", .{direction});
        var x, var y = getXYfromPos(start);
        var newPos: i8 = start;
        // running the iteration extra time cuz there is no do while loops
        while (true) {
            x += direction[0];
            y += direction[1];
            newPos = y * 8 + x;
            if (!isOnBoard(x, y) or self.board[intCast(usize, newPos)].color() == color) break;

            std.debug.print("x: {d}, y: {d}\n", .{ x, y });
            self.addLegalMove(Move{ .from = start, .to = intCast(u6, newPos) });
        }
    }
}

fn getLegalMoves(self: *Self, side: Piece.Color) void {
    for (self.board, 0..) |piece, i| {
        if (piece.color() != side) continue;
        const pos = intCast(u6, i);
        switch (piece) {
            .Wrook, .Brook => self.addDirections(pos, &.{
                .{ 1, 0 },
                .{ 0, 1 },
                .{ -1, 0 },
                .{ 0, -1 },
            }),
            .Wbishop, .Bbishop => self.addDirections(pos, &.{
                .{ 1, 1 },
                .{ -1, 1 },
                .{ -1, -1 },
                .{ 1, -1 },
            }),
            .Wqueen, .Bqueen => self.addDirections(pos, &.{
                .{ 1, 0 },
                .{ 0, 1 },
                .{ -1, 0 },
                .{ 0, -1 },
                .{ 1, 1 },
                .{ -1, 1 },
                .{ -1, -1 },
                .{ 1, -1 },
            }),
            .Wknight, .Bknight => {
                const x, const y = getXYfromPos(pos);
                for ([_][2]i8{ .{ 2, 1 }, .{ 2, -1 }, .{ -2, 1 }, .{ -2, -1 }, .{ 1, 2 }, .{ 1, -2 }, .{ -1, 2 }, .{ -1, -2 } }) |value| {
                    if (isOnBoard(x + value[0], y + value[1]) and self.board[intCast(usize, (y + value[1]) * 8 + x)].color() != side) self.addLegalMove(Move{ .from = pos, .to = intCast(u6, (y + value[1]) * 8 + x) });
                }
            },
            else => {},
        }
    }
}

test "getLegalMoves" {
    var board = fromFen("8/8/8/8/4N3/8/8/8");

    board.getLegalMoves(.White);
    for ([_]u12{ 2852, 1828, 2852, 1828, 3364, 1316, 3364, 1316 }) |value| {
        try testing.expect(board.legalMoves[value]);
    }
}
