"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var zod_1 = require("zod");
var zod_to_json_schema_1 = require("zod-to-json-schema");
var yargs = require("yargs");
var fs = require("fs");
var datetime = zod_1.z.string().datetime({ offset: true });
var activities = zod_1.z.array(zod_1.z.discriminatedUnion("type", [
    zod_1.z.object({
        type: zod_1.z.literal("clock"),
        start: datetime,
        end: datetime,
        title: zod_1.z.string(),
        link: zod_1.z.nullable(zod_1.z.string().url()),
        rawTitle: zod_1.z.string(),
        orgFile: zod_1.z.string(),
        orgId: zod_1.z.nullable(zod_1.z.string()),
        orgTags: zod_1.z.array(zod_1.z.string()),
    }),
    zod_1.z.object({
        type: zod_1.z.literal("gap"),
        start: datetime,
        end: datetime,
    }),
]));
var timeline = zod_1.z.array(zod_1.z.discriminatedUnion("type", [
    zod_1.z.object({
        type: zod_1.z.literal("block"),
        start: datetime,
        end: datetime,
        activities: zod_1.z.nullable(activities),
        title: zod_1.z.string(),
        state: zod_1.z.nullable(zod_1.z.string()),
        group: zod_1.z.nullable(zod_1.z.array(zod_1.z.nullable(zod_1.z.string()))),
        orgProperties: zod_1.z.record(zod_1.z.string()),
        orgTags: zod_1.z.array(zod_1.z.string()),
        orgText: zod_1.z.nullable(zod_1.z.string()),
    }),
    zod_1.z.object({
        type: zod_1.z.literal("anonymous"),
        start: datetime,
        end: datetime,
        activities: activities,
    }),
    zod_1.z.object({
        type: zod_1.z.literal("gap"),
        start: datetime,
        end: datetime,
    }),
    zod_1.z.object({
        type: zod_1.z.literal("away"),
        start: datetime,
        end: datetime,
    }),
    zod_1.z.object({
        type: zod_1.z.literal("idle"),
        start: datetime,
        end: datetime,
    }),
]));
var document = zod_1.z
    .object({
    version: zod_1.z.literal("0.1"),
    meta: zod_1.z.object({
        exportedAt: datetime,
        identity: zod_1.z.string(),
    }),
    body: zod_1.z.array(zod_1.z.discriminatedUnion("type", [
        zod_1.z.object({
            type: zod_1.z.literal("day"),
            date: zod_1.z.string().regex(/^\d{4}-\d{2}-\d{2}$/),
            start: zod_1.z.nullable(datetime),
            end: zod_1.z.nullable(datetime),
            timeline: timeline,
        }),
        zod_1.z.object({
            type: zod_1.z.literal("gap"),
            start: datetime,
            end: datetime,
            timeline: timeline,
        }),
    ])),
})
    .describe("Document exported using `org-memento-export-to-json` command.");
yargs
    .usage("Usage: $0 <cmd> [args]")
    .command("validate", "Validate a document", function (y) {
    return y
        .option("file", {
        type: "string",
        describe: "File to validate",
        demandOption: true,
    })
        .alias("f", "file");
}, function (argv) {
    var file = argv.file;
    fs.readFile(file, "utf-8", function (err, raw) {
        if (err) {
            console.error(err);
            return;
        }
        var data = JSON.parse(raw);
        var result = document.safeParse(data);
        if (!result.success) {
            // @ts-ignore
            console.error(result.error.issues);
        }
    });
})
    .command("schema", "Print the JSON schema", function (y) {
    return y;
}, function (_) {
    console.log(JSON.stringify((0, zod_to_json_schema_1.default)(document, {
        $refStrategy: "relative",
    })));
})
    .parse();
