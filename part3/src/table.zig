const std = @import("std");

const _value = @import("value.zig");
const _object = @import("object.zig");
const _memory = @import("memory.zig");

const Value = _value.Value;
const ObjString = _object.ObjString;
const Obj = _object.Obj;
const allocate = _memory.allocate;
const freeArray = _memory.freeArray;
const growCapacity = _memory.growCapacity;

const TABLE_MAX_LOAD = 0.75;

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

pub const Table = struct {
    const Self = @This();

    count: usize,
    entries: []Entry,
    // capacity: self.entries.len,

    fn _init(self: *Self) void {
        self.count = 0;
        self.entries = &[_]Entry{};
    }

    pub fn init() Self {
        var self = Self{
            .count = undefined,
            .entries = undefined,
        };
        self._init();
        return self;
    }

    pub fn deinit(self: *Self) void {
        freeArray(Entry, self.entries);
        self._init();
    }

    fn findEntry(self: *Self, key: *ObjString) *Entry {
        const capacity = self.entries.len;
        var index = key.hash & (capacity - 1);
        var tombstone: ?*Entry = null;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |entryKey| {
                if (entryKey == key) {
                    // We found the key.
                    return entry;
                }
            } else {
                if (entry.value.isNil()) {
                    // Empty entry.
                    if (tombstone) |tomb| {
                        return tomb;
                    } else {
                        return entry;
                    }
                } else {
                    // We found a tombstone.
                    if (tombstone == null) {
                        tombstone = entry;
                    }
                }
            }

            index = (index + 1) & (capacity - 1);
        }
    }

    pub fn findString(self: *Self, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) {
            return null;
        }
        const capacity = self.entries.len;
        var index = hash & (capacity - 1);
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |key| {
                if (key.chars.len == chars.len and
                    key.hash == hash and
                    std.mem.eql(u8, key.chars, chars))
                {
                    // We found it.
                    return entry.key;
                }
            } else {
                // Stop if we find an empty non-tombstone entry.
                if (entry.value.isNil()) {
                    return null;
                }
            }
            index = (index + 1) & (capacity - 1);
        }
    }

    pub fn removeWhite(self: *Self) void {
        var i: usize = 0;
        while (i < self.entries.len) : (i += 1) {
            var entry = self.entries[i];
            if (entry.key) |key| {
                if (!key.obj.isMarked) {
                    _ = self.delete(key);
                }
            }
        }
    }

    pub fn mark(self: *Self) void {
        // std.log.debug("DBG table.mark(). len = {d}", .{self.entries.len});
        var i: usize = 0;
        while (i < self.entries.len) : (i += 1) {
            var entry = &self.entries[i];
            if (entry.key) |key| {
                key.obj.mark();
            }
            entry.value.mark();
        }
    }

    fn adjustCapacity(self: *Self, capacity: usize) void {
        var entries = allocate(Entry, capacity);
        for (entries) |*entry| {
            entry.key = null;
            entry.value = Value.initNil();
        }
        const oldEntries = self.entries;
        self.entries = entries;
        self.count = 0;
        for (oldEntries) |*entry| {
            if (entry.key) |key| {
                var dest = self.findEntry(key);
                dest.key = entry.key;
                dest.value = entry.value;
                self.count += 1;
            } else {
                continue;
            }
        }
        freeArray(Entry, oldEntries);
    }

    pub fn set(self: *Self, key: *ObjString, value: Value) bool {
        if (@intToFloat(f32, self.count + 1) > @intToFloat(f32, self.entries.len) * TABLE_MAX_LOAD) {
            const capacity = growCapacity(self.entries.len);
            self.adjustCapacity(capacity);
        }

        var entry = self.findEntry(key);
        const isNewKey = entry.key == null;
        if (isNewKey and entry.value.isNil()) {
            self.count += 1;
        }
        entry.key = key;
        entry.value = value;
        return isNewKey;
    }

    pub fn get(self: *Self, key: *ObjString, value: *Value) bool {
        if (self.count == 0) {
            return false;
        }
        const entry = self.findEntry(key);
        if (entry.key == null) {
            return false;
        }
        value.* = entry.value;
        return true;
    }

    pub fn delete(self: *Self, key: *ObjString) bool {
        if (self.count == 0) {
            return false;
        }

        // Find the entry.
        var entry = self.findEntry(key);
        if (entry.key == null) {
            return false;
        }

        // Place a tombstone in the entry.
        entry.key = null;
        entry.value = Value.initBool(true);
        return true;
    }

    pub fn addAll(self: *Self, from: *Self) void {
        for (from.entries) |*entry| {
            if (entry.key) |key| {
                _ = self.set(key, entry.value);
            }
        }
    }
};
