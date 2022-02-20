const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn();
    defer stdin.close();

    var depths = std.ArrayList(u16).init(allocator);
    defer depths.deinit();

    while (true) {
        const line = try stdin.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', 5);
        if (line) |l| {
            const depth = try std.fmt.parseUnsigned(u16, l, 10);
            try depths.append(depth);
        } else {
            break;
        }
    }

    var increases_count: u16 = 0;
    var previous_depth: ?u16 = null;

    for (depths.items) |current_depth| {
        if (previous_depth) |prev| {
            if (current_depth > prev) {
                increases_count += 1;
            }
        }
        previous_depth = current_depth;
    }

    std.debug.print("Increases: {d}\n", .{increases_count});
}
