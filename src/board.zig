//! the code for the chessboard.

const std = @import("std");
const testing = @import("std").testing;

const Self = @This();

/// a chess piece.
pub const Piece = enum {
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

    /// a chess piece color.
    pub const Color = enum(u1) {
        White,
        Black,
        inline fn other(self: Color) Color {
            return if (self == .Black) .White else .Black;
        }
    };
    /// get a chess piece color from a piece.
    /// .empty = null.
    fn color(self: Piece) ?Color {
        if (self == .empty) return null;
        return switch (self) {
            .Wking, .Wqueen, .Wrook, .Wbishop, .Wknight, .Wpawn => .White,
            else => .Black,
        };
    }
};

/// converting numbers.
/// trust me, its REALLY useful.
inline fn intCast(comptime T: type, int: anytype) T {
    return @as(T, @intCast(int));
}

/// finds an index of an item.
/// also can be used to find if a list contains an item.
pub fn indexOf(comptime T: type, haystack: []const T, needle: T) error{NoNeedleFound}!usize {
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
/// converts a squere notation like `a1`
/// to its respective index on the board, so 56
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

    /// converts the long algebraic notation to a move.
    pub fn fromNotation(notation: []const u8, color: Piece.Color) error{ NotAParsableMove, NoNeedleFound, WrongLength }!Move {
        var split_notation = std.mem.splitAny(u8, notation, " -xX");
        var from, var to = .{ split_notation.first(), split_notation.rest() };
        if (to.len == 0 and notation.len > 2) {
            // the move is of type "e2e4"
            // instead of "e2-e4"
            to = from[from.len-2..];
            from = from[0..from.len-2];
        }
        // yes you could do "O GAY"
        // or even "O  "
        // or even "OXXXO"
        // but i don't care enough to fix it
        // plus its funny
        if (from.len == 1 and (from[0] == 'O' or from[0] == '0')) {
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
        // its just to ignore the piece type, so you can actually just do "Id4 d5"
        if (from.len == 3) return fromNotation(notation[1..], color);

        if (from.len != 2 or to.len != 2) {
            return error.NotAParsableMove;
        }

        return Move{ .from = try squereToInt(from), .to = try squereToInt(to) };
    }

    /// converts a `Move` to an integer
    pub inline fn toInt(self: Move) u12 {
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
    // no space
    try testing.expectEqual(
        Move{ .from = try squereToInt("e2"), .to = try squereToInt("e4") },
        try Move.fromNotation("e2e4", .White),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("d7"), .to = try squereToInt("d5") },
        try Move.fromNotation("d7d5", .Black),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("f6"), .to = try squereToInt("e4") },
        try Move.fromNotation("f6e4", .Black),
    );
    try testing.expectEqual(
        Move{ .from = try squereToInt("g1"), .to = try squereToInt("f3") },
        try Move.fromNotation("g1f3", .White),
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
/// The board itself
board: [64]Piece = undefined,
/// The move history, apperently the largest possible chess game is 8455 moves or something
move_history: [9e3]?Move = @splat(null),
/// the legal moves array.
/// 4096 = 2^@bitSizeOf(Move)
/// intented usage: legal_moves[move.toInt()]
legal_moves: [4096]bool = undefined,

/// converts a FEN Notation into a board.
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
/// forcably makes a move happen on the board.
/// also adds this move to the `move_history`.
pub fn forceMakeMove(self: *Self, move: Move) void {
    const piece = self.*.board[move.from];
    self.board[move.from] = .empty;
    self.board[move.to] = piece;
    self.addMove(move);
}
/// replaces a piece on the board.
/// useful for en passant and moves that don't get added to the `move_history` list, like castles
pub fn replacePiece(self: *Self, pos: u6, with: Piece) void {
    self.board[pos] = with;
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
/// adds a move to the `move_history` list.
fn addMove(self: *Self, move: Move) void {
    self.move_history[self.firstFreeMove()] = move;
}

test "addMove" {
    var board = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");

    // Test adding a single move
    const move1 = Move{ .from = try squereToInt("e2"), .to = try squereToInt("e4") };
    board.addMove(move1);
    try testing.expectEqual(move1, board.move_history[0].?);
    try testing.expectEqual(@as(?Move, null), board.move_history[1]);

    // Test adding multiple moves
    const move2 = Move{ .from = try squereToInt("e7"), .to = try squereToInt("e5") };
    board.addMove(move2);
    try testing.expectEqual(move2, board.move_history[1].?);
    try testing.expectEqual(@as(?Move, null), board.move_history[2]);

    // Test that remaining slots are null
    try testing.expectEqual(@as(?Move, null), board.move_history[3]);
    try testing.expectEqual(@as(?Move, null), board.move_history[100]);
}

/// adds a legal move to the `legal_moves` list.
fn addLegalMove(self: *Self, move: Move) void {
    self.legal_moves[move.toInt()] = true;
}
/// gets the X and the Y of a position on the board.
/// intended usage: `const x, const y = getXYfromPos(pos);`
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
/// checks if X and Y match a position on the board.
inline fn isOnBoard(x: i8, y: i8) bool {
    return x >= 0 and x < 8 and y >= 0 and y < 8;
}

/// adds moves in a couple of directions.
/// useful for:
///   - rook
///   - bishop
///   - queen
fn addDirections(self: *Self, start: u6, comptime directions: []const [2]i8) void {
    const color = self.board[start].color().?;
    for (directions) |direction| {
        var x, var y = getXYfromPos(start);
        var newPos: i8 = start;
        while (true) {
            x += direction[0];
            y += direction[1];
            newPos = y * 8 + x;
            if (!isOnBoard(x, y) or self.board[@intCast(newPos)].color() == color) break;
            self.addLegalMove(Move{ .from = start, .to = @intCast(newPos) });
        }
    }
}
/// checks the color of a piece on a squere.
/// its more useful then it seems.
inline fn checkColor(self: Self, squere: anytype) error{SquereOutsideBoard}!?Piece.Color {
    return if (squere < 0 or squere > 63) error.SquereOutsideBoard else self.board[@intCast(squere)].color();
}

/// returns the index of the first free move in the `move_history`.
pub fn firstFreeMove(self: Self) usize {
    return blk: for (self.move_history, 0..) |value, i| {
        if (value == null) break :blk i;
    } else unreachable;
}
/// checks if the piece on the given position moved.
fn hasPieceMoved(self: Self, piece_pos: u6) bool {
    return for (self.move_history) |move| {
        if (move == null) break false;
        if (move.?.to == piece_pos) break true;
    } else unreachable;
}

test "hasPieceMoved basic tests" {
    var board = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");

    // Test initial position - no moves
    try testing.expect(!board.hasPieceMoved(try squereToInt("e2")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e7")));

    // Test after making moves
    board.forceMakeMove(Move{ .from = try squereToInt("e2"), .to = try squereToInt("e4") });
    try testing.expect(board.hasPieceMoved(try squereToInt("e4")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e2")));

    board.forceMakeMove(Move{ .from = try squereToInt("e7"), .to = try squereToInt("e5") });
    try testing.expect(board.hasPieceMoved(try squereToInt("e5")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e7")));

    // Test piece capture
    board.forceMakeMove(Move{ .from = try squereToInt("e4"), .to = try squereToInt("e5") });
    try testing.expect(board.hasPieceMoved(try squereToInt("e5")));
}

test "hasPieceMoved castling tests" {
    var board = fromFen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R");

    // Test initial rook and king positions
    try testing.expect(!board.hasPieceMoved(try squereToInt("e1")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("a1")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("h1")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e8")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("a8")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("h8")));

    // Test after castling
    board.forceMakeMove(Move{ .from = try squereToInt("e1"), .to = try squereToInt("g1") });
    try testing.expect(board.hasPieceMoved(try squereToInt("g1")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e1")));
}

test "hasPieceMoved pawn tests" {
    var board = fromFen("8/pppppppp/8/8/8/8/PPPPPPPP/8");

    // Test initial pawn positions
    try testing.expect(!board.hasPieceMoved(try squereToInt("e2")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e7")));

    // Test after single move
    board.forceMakeMove(Move{ .from = try squereToInt("e2"), .to = try squereToInt("e3") });
    try testing.expect(board.hasPieceMoved(try squereToInt("e3")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e2")));

    // Test after double move
    board.forceMakeMove(Move{ .from = try squereToInt("e7"), .to = try squereToInt("e5") });
    try testing.expect(board.hasPieceMoved(try squereToInt("e5")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("e7")));
}

test "hasPieceMoved untouched pieces" {
    var board = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");

    // Make some moves
    board.forceMakeMove(Move{ .from = try squereToInt("e2"), .to = try squereToInt("e4") });
    board.forceMakeMove(Move{ .from = try squereToInt("e7"), .to = try squereToInt("e5") });
    board.forceMakeMove(Move{ .from = try squereToInt("g1"), .to = try squereToInt("f3") });

    // Test untouched pieces
    try testing.expect(!board.hasPieceMoved(try squereToInt("d2")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("d7")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("a1")));
    try testing.expect(!board.hasPieceMoved(try squereToInt("h8")));
}

/// populates the `legal_moves` list.
pub fn getLegalMoves(self: *Self, side: Piece.Color) void {
    for (self.board, 0..) |piece, i| {
        if (piece.color() != side) continue;
        const pos: u6 = @intCast(i);
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
                for ([_][2]i8{
                    .{ 2, 1 },
                    .{ 2, -1 },
                    .{ -2, 1 },
                    .{ -2, -1 },
                    .{ 1, 2 },
                    .{ 1, -2 },
                    .{ -1, 2 },
                    .{ -1, -2 },
                }) |value| {
                    const new_pos = (y + value[1]) * 8 + (x + value[0]);
                    if (isOnBoard(x + value[0], y + value[1]) and self.checkColor(new_pos) catch unreachable != side) {
                        self.addLegalMove(Move{ .from = pos, .to = @intCast(new_pos) });
                    }
                }
            },
            .Wpawn, .Bpawn => {
                const direction: i6 = if (side == .Black) 1 else -1;
                // classic movement
                var sq: i8 = intCast(i8, pos) + direction * 8;
                if (self.checkColor(sq) catch continue == null) {
                    const new_move1 = Move{ .from = pos, .to = @intCast(sq) };
                    self.addLegalMove(new_move1);
                    sq += direction * 8;
                    // double movement
                    if (self.checkColor(sq) catch continue == null and switch (side) {
                        .White => pos >= 48 and pos < 56,
                        .Black => pos >= 8 and pos < 16,
                    }) {
                        const new_move2 = Move{ .from = pos, .to = @intCast(sq) };
                        self.addLegalMove(new_move2);
                    }
                }

                // capture
                const opposite_side: Piece.Color = side.other();
                if (pos % 8 != 0 and self.checkColor(intCast(i7, pos) + direction * 8 - 1) catch unreachable == opposite_side) {
                    const move = Move{ .from = pos, .to = @intCast(intCast(i7, pos) + direction * 8 - 1) };
                    self.addLegalMove(move);
                }
                if (pos % 8 != 7 and self.checkColor(intCast(i7, pos) + direction * 8 + 1) catch unreachable == opposite_side) {
                    const move = Move{ .from = pos, .to = @intCast(intCast(i7, pos) + direction * 8 + 1) };
                    self.addLegalMove(move);
                }
                const opposite_piece: Piece = if (opposite_side == .Black) .Bpawn else .Wpawn;
                if (self.firstFreeMove() == 0) continue;
                const last_move: Move = self.move_history[self.firstFreeMove() - 1].?;
                const did_double_move: bool = intCast(i7, last_move.from) - intCast(i7, last_move.to) == direction * 16;
                if (!did_double_move) continue;
                if (pos % 8 != 0 and self.board[pos - 1] == opposite_piece and last_move.to == pos - 1) {
                    const move = Move{ .from = pos, .to = @intCast(intCast(i7, pos) + direction * 8 - 1) };
                    self.addLegalMove(move);
                }
                if (pos % 8 != 7 and self.board[pos + 1] == opposite_piece and last_move.to == pos + 1) {
                    const move = Move{ .from = pos, .to = @intCast(intCast(i7, pos) + direction * 8 + 1) };
                    self.addLegalMove(move);
                }
            },
            .Bking, .Wking => {
                const x, const y = getXYfromPos(pos);
                for ([_][2]i8{
                    .{ 1, 0 },
                    .{ 0, 1 },
                    .{ -1, 0 },
                    .{ 0, -1 },
                    .{ -1, -1 },
                    .{ 1, 1 },
                    .{ 1, -1 },
                    .{ -1, 1 },
                }) |value| {
                    const new_pos: i8 = (y + value[1]) * 8 + (x + value[0]);
                    if (isOnBoard(x + value[0], y + value[1]) and self.checkColor(new_pos) catch unreachable != side) {
                        self.addLegalMove(Move{ .from = pos, .to = @intCast(new_pos) });
                    }
                }
                // castles.
                const start_pos = if (side == .White) comptime squereToInt("e1") catch unreachable else comptime squereToInt("e8") catch unreachable;
                const rook_color: Piece = if (side == .White) .Wrook else .Brook;
                if (pos != start_pos or self.hasPieceMoved(pos)) continue;
                if (
                // and are the squeres to O-O free??
                self.checkColor(pos + 1) catch unreachable == null and self.checkColor(pos + 2) catch unreachable == null
                    // and did the rook move??
                and self.board[pos + 3] == rook_color and !self.hasPieceMoved(pos + 3)) {
                    const move = Move{ .from = pos, .to = pos + 2 };
                    self.addLegalMove(move);
                }

                if (
                // and are the squeres to O-O-O free??
                self.checkColor(pos - 1) catch unreachable == null and self.checkColor(pos - 2) catch unreachable == null
                    // and did the rook move??
                and self.board[pos - 4] == rook_color and !self.hasPieceMoved(pos - 4)) {
                    const move = Move{ .from = pos, .to = pos - 2 };
                    self.addLegalMove(move);
                }
            },
            .empty => unreachable,
        }
    }
}

test "legal moves king basic movements" {
    // Test basic king movements in all 8 directions
    var board = fromFen("8/8/8/3K4/8/8/8/8");
    board.getLegalMoves(.White);

    // Test all adjacent squares
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-d6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-e6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-e5", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-e4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-d4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-c4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-c5", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-c6", .White)).toInt()]);
}

test "legal moves king captures" {
    // Test king captures of opposite color pieces
    var board = fromFen("8/8/2ppp3/2pKp3/2ppp3/8/8/8");
    board.getLegalMoves(.White);

    // Test all capture moves
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xc6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xd6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xe6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xe5", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xe4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xd4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xc4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5xc5", .White)).toInt()]);
}

test "legal moves king castling" {
    // Test castling moves
    var board = fromFen("8/8/8/8/8/8/8/R3K2R");
    board.getLegalMoves(.White);

    // Test kingside castle
    try testing.expect(board.legal_moves[(try Move.fromNotation("O-O", .White)).toInt()]);

    // Test queenside castle
    try testing.expect(board.legal_moves[(try Move.fromNotation("O-O-O", .White)).toInt()]);

    // Test castling blocked by pieces
    board = fromFen("8/8/8/8/8/8/8/R1N1K1BR");
    board.getLegalMoves(.White);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("O-O", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("O-O-O", .White)).toInt()]);
}

test "legal moves king blocked moves" {
    // Test king blocked by friendly pieces
    var board = fromFen("8/8/2PPP3/2PKP3/2PPP3/8/8/8");
    board.getLegalMoves(.White);

    // Test all blocked moves
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-c6", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-d6", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-e6", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-e5", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-e4", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-d4", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-c4", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("d5-c5", .White)).toInt()]);
}

test "legal moves pawn tests capture moves" {
    var board = fromFen("8/8/8/3p4/4P3/8/8/8");
    board.getLegalMoves(.White);
    board.getLegalMoves(.Black);
    const white_cap_left = try Move.fromNotation("e4xd5", .White);
    const black_cap_right = try Move.fromNotation("d5xe4", .Black);
    try testing.expect(board.legal_moves[white_cap_left.toInt()]);
    try testing.expect(board.legal_moves[black_cap_right.toInt()]);

    board = fromFen("8/8/8/pp6/PP6/8/8/8");
    board.getLegalMoves(.White);
    board.getLegalMoves(.Black);
    const white_edge_cap = try Move.fromNotation("a4xb5", .White);
    const black_edge_cap = try Move.fromNotation("a5xb4", .Black);
    try testing.expect(board.legal_moves[white_edge_cap.toInt()]);
    try testing.expect(board.legal_moves[black_edge_cap.toInt()]);

    board = fromFen("8/8/3p4/3p4/4P3/8/8/8");
    board.getLegalMoves(.White);
    board.getLegalMoves(.Black);
    const blocked_white_cap = try Move.fromNotation("e4xd5", .White);
    try testing.expect(board.legal_moves[blocked_white_cap.toInt()]);

    // Invalid captures - empty diagonal squares
    board = fromFen("8/8/8/8/4P3/8/8/8");
    board.getLegalMoves(.White);
    const empty_diag_cap = try Move.fromNotation("e4xd5", .White);
    try testing.expect(!board.legal_moves[empty_diag_cap.toInt()]);

    // Invalid captures - same color pieces
    board = fromFen("8/8/8/3P4/4P3/8/8/8");
    board.getLegalMoves(.White);
    const same_color_cap = try Move.fromNotation("e4xd5", .White);
    try testing.expect(!board.legal_moves[same_color_cap.toInt()]);

    // Invalid captures - non-diagonal squares
    board = fromFen("8/8/8/4p3/4P3/8/8/8");
    board.getLegalMoves(.White);
    const non_diag_cap = try Move.fromNotation("e4xe5", .White);
    try testing.expect(!board.legal_moves[non_diag_cap.toInt()]);
}

test "legal moves pawn tests classic moves" {
    var board = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");
    board.getLegalMoves(.White);
    board.getLegalMoves(.Black);
    // white
    for (48..56) |i| {
        const move = Move{ .from = @intCast(i), .to = @intCast(i - 8) };
        try testing.expect(board.legal_moves[move.toInt()]);
    }
    // black
    for (8..16) |i| {
        const move = Move{ .from = @intCast(i), .to = @intCast(i + 8) };
        try testing.expect(board.legal_moves[move.toInt()]);
    }
    var falsy_board = fromFen("rnbqkbnr/8/8/8/8/pppppppp/PPPPPPPP/RNBQKBNR");
    falsy_board.getLegalMoves(.Black);
    falsy_board.getLegalMoves(.White);
    // white
    for (48..56) |i| {
        const move = Move{ .from = @intCast(i), .to = @intCast(i - 8) };
        try testing.expect(!falsy_board.legal_moves[move.toInt()]);
    }
    // black
    for (40..48) |i| {
        const move = Move{ .from = @intCast(i), .to = @intCast(i + 8) };
        try testing.expect(!falsy_board.legal_moves[move.toInt()]);
    }
}
test "legal moves pawn tests double moves" {
    var board = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");
    board.getLegalMoves(.White);
    board.getLegalMoves(.Black);
    // white double moves from starting position
    for (48..56) |i| {
        const move = Move{ .from = @intCast(i), .to = @intCast(i - 16) };
        try testing.expect(board.legal_moves[move.toInt()]);
    }
    // black double moves from starting position
    for (8..16) |i| {
        const move = Move{ .from = @intCast(i), .to = @intCast(i + 16) };
        try testing.expect(board.legal_moves[move.toInt()]);
    }
    board = fromFen("8/8/p7/P7/8/8/8/8");
    board.getLegalMoves(.White);
    board.getLegalMoves(.Black);
    const no_white_double = Move{ .from = @intCast(32), .to = @intCast(16) };
    try testing.expect(!board.legal_moves[no_white_double.toInt()]);
    const no_black_double = Move{ .from = @intCast(40), .to = @intCast(56) };
    try testing.expect(!board.legal_moves[no_black_double.toInt()]);
    board = fromFen("rnbqkbnr/pppppppp/8/8/P7/8/1PPPPPPP/RNBQKBNR");
    board.getLegalMoves(.White);
    const blocked_white = Move{ .from = @intCast(48), .to = @intCast(32) };
    try testing.expect(!board.legal_moves[blocked_white.toInt()]);

    board = fromFen("rnbqkbnr/1ppppppp/8/p7/8/8/PPPPPPPP/RNBQKBNR");
    board.getLegalMoves(.Black);
    const blocked_black = Move{ .from = @intCast(9), .to = @intCast(25) };
    try testing.expect(board.legal_moves[blocked_black.toInt()]);
}

test "legal moves knight basic movements" {
    var board = fromFen("8/8/8/3N4/8/8/8/8");
    board.getLegalMoves(.White);

    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-e7", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-f6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-f4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-e3", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-c3", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-b4", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-b6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("d5-c7", .White)).toInt()]);
}

test "legal moves knight captures" {
    var board = fromFen("8/8/2p1p3/4N3/2p1p3/8/8/8");
    board.getLegalMoves(.White);

    try testing.expect(board.legal_moves[(try Move.fromNotation("e5xc6", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("e5xc4", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("e5xe6", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("e5xe4", .White)).toInt()]);
}

test "legal moves knight blocked" {
    var board = fromFen("8/8/2P1P3/4N3/2P1P3/8/8/8");
    board.getLegalMoves(.White);

    try testing.expect(!board.legal_moves[(try Move.fromNotation("e5-c6", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("e5-c4", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("e5-e6", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("e5-e4", .White)).toInt()]);
}

test "legal moves knight edge moves" {
    var board = fromFen("8/8/8/8/8/8/8/N7");
    board.getLegalMoves(.White);

    try testing.expect(board.legal_moves[(try Move.fromNotation("a1-b3", .White)).toInt()]);
    try testing.expect(board.legal_moves[(try Move.fromNotation("a1-c2", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("a1-b4", .White)).toInt()]);
    try testing.expect(!board.legal_moves[(try Move.fromNotation("a1-c3", .White)).toInt()]);
}

fn isAttacked(self: Self, pos: u6) bool {
    const color: Piece.Color = (self.checkColor(pos) catch unreachable ).?;
    for (self.legal_moves, 0..) |isLegal, id| {
        if (!isLegal) continue;
        const move: Move = @bitCast(intCast(u12, id));
        if (move.to != pos) continue;
        if (self.checkColor(move.from) catch unreachable == color) continue;
        return true;
    }
    return false;
}

test "isAttacked fool's mate" {
    var board = fromFen("rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR");
    board.getLegalMoves(.Black);
    try testing.expect(board.isAttacked(try squereToInt("e1")));
}

test "isAttacked normal mate" {
    var board = fromFen("r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR");
    board.getLegalMoves(.White);
    try testing.expect(board.isAttacked(try squereToInt("e8")));
}

test "isAttacked false positive" {
    var board = fromFen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR");
    board.getLegalMoves(.White);
    try testing.expect(!board.isAttacked(try squereToInt("e8")));
}

fn canMove(self: Self, from: u6) bool {

    for (self.legal_moves, 0..) |isLegal, id| {
        if (!isLegal) continue;
        const move: Move = @bitCast(intCast(u12, id));
        if (move.from != from) continue;
        return true;
    }
    return false;
}

test "canMove basic tests" {
    var board = fromFen("8/8/8/3P4/8/8/8/8");
    board.getLegalMoves(.White);
    try testing.expect(board.canMove(try squereToInt("d5")));

    board = fromFen("8/8/8/8/8/8/P7/8");
    board.getLegalMoves(.White);
    try testing.expect(board.canMove(try squereToInt("a2")));
}

test "canMove blocked piece tests" {
    var board = fromFen("8/8/3p4/3P4/8/8/8/8");
    board.getLegalMoves(.White);
    try testing.expect(!board.canMove(try squereToInt("d5")));

    board = fromFen("8/8/8/3p4/3P4/8/8/8");
    board.getLegalMoves(.White);
    try testing.expect(!board.canMove(try squereToInt("d4")));
}

test "canMove edge cases" {
    var board = fromFen("8/8/8/8/8/8/8/8");
    board.getLegalMoves(.White);
    try testing.expect(!board.canMove(try squereToInt("a1")));

    board = fromFen("k7/8/8/8/8/8/8/K7");
    board.getLegalMoves(.White);
    try testing.expect(board.canMove(try squereToInt("a1")));
    board.getLegalMoves(.Black);
    try testing.expect(board.canMove(try squereToInt("a8")));
}

pub fn isCheckmated(self: Self, kingPos: u6) bool {
    return !self.canMove(kingPos) and self.isAttacked(kingPos);
}