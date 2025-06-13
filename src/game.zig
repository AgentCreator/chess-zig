//! the main game file.
//! its a *Facade* on top of the board.

pub const Board = @import("board");
const std = @import("std");
const Self = @This();

/// converting numbers.
/// trust me, its REALLY useful.
inline fn intCast(comptime T: type, int: anytype) T {
    return @as(T, @intCast(int));
}

const BoardStartingPosition = union(enum) {
    Classic,
    /// takes in a FEN
    Custom: []const u8,
};

board: Board,
currentSide: Board.Piece.Color,

pub fn init(comptime pos: BoardStartingPosition) Self {
    return Self{.board = Board.fromFen(switch (pos) {
        .Classic => "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
        .Custom => |p| p,
    }), .currentSide = .White };
}

pub fn playMove(self: *Self, move: Board.Move) error{ImpossibleMove}!void {
    self.board.legalMoves = @splat(false);
    self.board.getLegalMoves(self.currentSide);
    defer self.currentSide = if (self.currentSide == Board.Piece.Color.White) Board.Piece.Color.Black else Board.Piece.Color.White;
    if (!self.board.legalMoves[move.toInt()]) return error.ImpossibleMove;
    const piece = self.board.board[move.from];
    self.board.forceMakeMove(move);
    if (piece != .Wking and piece != .Bking and piece != .Wpawn and piece != .Bpawn) return;
    const diff = intCast(i8, move.to) - intCast(i8, move.from);
    if ((piece == .Wking or piece == .Bking) and @abs(diff) == 2) {
        // castles!!
        const rook_pos = if (diff == 2) move.to + 1 else move.to - 2;
        const new_rook_pos = if (diff == 2) move.to - 1 else move.to + 1;
        const rook_type = self.board.board[rook_pos];
        self.board.replacePiece(new_rook_pos, rook_type);
        self.board.replacePiece(rook_pos, .empty);
    }
    if ((piece == .Wpawn or piece == .Bpawn) and @abs(diff) != 8 and self.board.board[move.to] == .empty) {
        // en passant!
        if (@abs(diff) == 7) self.board.replacePiece(move.from + 1, .empty)
            // a little comment, just so the formatter doesn't put the next line on the same line
        else self.board.replacePiece(move.from - 1, .empty);
    }
}

test "hehe" {
    var chessboard = init(.Classic);
    try chessboard.prettyPrint();
    try chessboard.playMove(try Board.Move.fromNotation("e2-e4", .White),);
    try chessboard.prettyPrint();
}

pub fn prettyPrint(self: Self) !void {
    const stderr = std.io.getStdErr().writer();
    inline for (self.board.board, 0..) |pieceType, i| {
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

        // std.debug.print("{u}{c}", .{ piece, chr });
        if (i % 8 == 0) {
            // std.debug.print("new layer!\n", .{});
            try stderr.print("{d} ", .{8 - @divFloor(i, 8)});
        }
        try stderr.print("{u}{c}", .{piece, chr});
    }
    try stderr.print("  A B C D E F G H\n", .{});
}

pub fn listenForValidMove(self: Self, allocator: std.mem.Allocator) !Board.Move {
    const stdin = std.io.getStdIn().reader();
    const stderr = std.io.getStdErr().writer();
    try stderr.print("{s}, your move!\n", .{if (self.currentSide == .Black) "White" else "Black"});
    const move = try stdin.readUntilDelimiterAlloc(allocator, '\n', 20);
    defer allocator.free(move);
    return Board.Move.fromNotation(move, self.currentSide) catch {
        try stderr.print("\"{s}\" is not a valid move. Remember, moves here are this kind of notation: 'Ng1-f3'. Try again!\n", .{move});
        return try listenForValidMove(self, allocator);
    };
}

pub fn listenAndPlayMove(self: *Self, allocator: std.mem.Allocator) !void {
    const stderr = std.io.getStdErr().writer();
    const move = try self.listenForValidMove(allocator);
    self.playMove(move) catch {
        try stderr.print("hey, so this move you played is actually impossible... yeah try again\n", .{});
        self.currentSide = if (self.currentSide == Board.Piece.Color.White) Board.Piece.Color.Black else Board.Piece.Color.White;
        try listenAndPlayMove(self, allocator);
    };
}

pub fn isGameEnd(self: *Self) error{EndGameError}!void {
    _ = (Board.indexOf(Board.Piece, &self.board.board, .Wking) catch return error.EndGameError);
    _ = (Board.indexOf(Board.Piece, &self.board.board, .Bking) catch return error.EndGameError);
}

//
pub fn mainLoop(self: *Self, allocator: std.mem.Allocator) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print("Move #{d}\n", .{self.board.firstFreeMove()});
    try self.prettyPrint();
    try self.listenAndPlayMove(allocator);
    try isGameEnd(self);
    try self.mainLoop(allocator);
}