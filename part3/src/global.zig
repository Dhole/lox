const _common = @import("common.zig");
const _vm = @import("vm.zig");

const Flags = _common.Flags;
const VM = _vm.VM;

pub const flags = Flags{ .debugTraceExecution = false, .debugPrintCode = false };

pub var vm: *VM(flags) = undefined;

pub fn setVM(v: *VM(flags)) void {
    vm = v;
}
