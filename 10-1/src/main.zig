const std = @import("std");

const allocator = std.heap.page_allocator;

pub fn main() !void {
    const file = @embedFile("../input.txt");
    var lines = std.mem.split(file, "\n");

    var score: u32 = 0;

    lines: while (lines.next()) |line| {
        var stack = std.ArrayList(u8).init(allocator);
        defer stack.deinit();

        for (line) |character| {
            switch (character) {
                '(', '[', '{', '<' => try stack.append(character),
                ')', ']', '}', '>' => {
                    const previous_character = stack.pop();

                    if (!isValidPair(previous_character, character)) {
                        score = score + getScore(character);
                        continue :lines;
                    }
                },
                else => unreachable
            }
        }
    }

    std.debug.print("{}", .{score});
}

fn getScore(character: u8) u32 {
    return switch (character) {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        else => unreachable
    };
}

fn isValidPair(open: u8, close: u8) bool {
    const expected_close: u8 = switch (open) {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        else => unreachable
    };

    return close == expected_close;
}
