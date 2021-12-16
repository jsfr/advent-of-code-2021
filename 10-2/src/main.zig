const std = @import("std");

const allocator = std.heap.page_allocator;

pub fn main() !void {
    const file = @embedFile("../input.txt");
    var lines = std.mem.split(file, "\n");

    var scores = std.ArrayList(u64).init(allocator);
    defer scores.deinit();

    lines: while (lines.next()) |line| {
        var stack = std.ArrayList(u8).init(allocator);
        defer stack.deinit();

        for (line) |character| {
            switch (character) {
                '(', '[', '{', '<' => try stack.append(character),
                ')', ']', '}', '>' => {
                    const previous_character = stack.pop();
                    if (!isValidPair(previous_character, character)) {
                        continue :lines;
                    }
                },
                else => unreachable,
            }
        }

        if (stack.items.len == 0) {
            continue :lines;
        }

        var score: u64 = 0;

        // line was valid as it survived the above check
        while (stack.popOrNull()) |character| {
            score = (score * 5) + getScore(character);
        }

        try scores.append(score);
    }

    std.sort.sort(u64, scores.items, {}, comptime std.sort.asc(u64));

    const index = @divFloor(scores.items.len, 2);

    std.debug.print("{} - {} - {}", .{ scores.items[index], index, scores.items.len });
}

fn getScore(character: u8) u64 {
    return switch (character) {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        else => unreachable,
    };
}

fn isValidPair(open: u8, close: u8) bool {
    const expected_close: u8 = switch (open) {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        else => unreachable,
    };

    return close == expected_close;
}
