const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn();
    defer stdin.close();
    const stdin_reader = stdin.reader();

    var buffer: [5]u8 = undefined;

    var increases_count: u16 = 0;
    var previous_depth: ?u16 = null;

    while (true) {
        const line = try stdin_reader.readUntilDelimiterOrEof(&buffer, '\n');
        if (line) |l| {
            const current_depth = try std.fmt.parseUnsigned(u16, l, 10);

            if (previous_depth) |prev| {
                if (current_depth > prev) {
                    increases_count += 1;
                }
            }

            previous_depth = current_depth;
        } else {
            break;
        }
    }

    std.debug.print("Increases: {d}\n", .{increases_count});
}
