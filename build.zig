const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const unicode_data_gen = b.addExecutable(.{
        .name = "unicode_data",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/gen/unicode_data.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const unicode_types = b.createModule(.{
        .root_source_file = b.path("src/gen/unicode_types.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_data_gen = b.addRunArtifact(unicode_data_gen);
    run_data_gen.step.dependOn(&unicode_data_gen.step);
    run_data_gen.addFileInput(.{ .cwd_relative = "src/gen/UnicodeData.txt" });

    const output_file = run_data_gen.addOutputFileArg("data.zig");
    const write_files = b.addWriteFiles();
    // _ = write_files.addCopyFile(b.path("src/gen/unicode.zig"), "src/gen/unicode.zig");

    const exe = b.addExecutable(.{
        .name = "iotac",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const unicode_data = b.createModule(.{
        .root_source_file =
            write_files.addCopyFile(output_file, "src/gen/data.zig"),
    });
    unicode_data.addImport("unicode_types", unicode_types);

    exe.root_module.addImport("unicode_types", unicode_types);
    exe.root_module.addImport("unicode_data", unicode_data);
    exe.step.dependOn(&write_files.step);

    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run iotac compiler");
    run_step.dependOn(&run_exe.step);
}
