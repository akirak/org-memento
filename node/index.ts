import { z } from "zod";
import zodToJsonSchema from "zod-to-json-schema";
import * as yargs from "yargs";
import * as fs from "fs";

const datetime = z.string().datetime({ offset: true });

const activities = z.array(
  z.discriminatedUnion("type", [
    z.object({
      type: z.literal("clock"),
      start: datetime,
      end: datetime,
      title: z.string(),
      link: z.nullable(z.string().url()),
      rawTitle: z.string(),
      orgFile: z.string(),
      orgId: z.nullable(z.string()),
      orgTags: z.array(z.string()),
    }),
    z.object({
      type: z.literal("gap"),
      start: datetime,
      end: datetime,
    }),
  ])
);

const timeline = z.array(
  z.discriminatedUnion("type", [
    z.object({
      type: z.literal("block"),
      start: datetime,
      end: datetime,
      activities: z.nullable(activities),
      title: z.string(),
      state: z.nullable(z.string()),
      group: z.nullable(z.array(z.nullable(z.string()))),
      orgProperties: z.record(z.string()),
      orgTags: z.array(z.string()),
      orgText: z.nullable(z.string()),
    }),
    z.object({
      type: z.literal("anonymous"),
      start: datetime,
      end: datetime,
      activities,
    }),
    z.object({
      type: z.literal("gap"),
      start: datetime,
      end: datetime,
    }),
    z.object({
      type: z.literal("away"),
      start: datetime,
      end: datetime,
    }),
    z.object({
      type: z.literal("idle"),
      start: datetime,
      end: datetime,
    }),
  ])
);

const document = z
  .object({
    version: z.literal("0.1"),
    meta: z.object({
      exportedAt: datetime,
      identity: z.string(),
    }),
    body: z.array(
      z.discriminatedUnion("type", [
        z.object({
          type: z.literal("day"),
          date: z.string().regex(/^\d{4}-\d{2}-\d{2}$/),
          start: z.nullable(datetime),
          end: z.nullable(datetime),
          timeline: timeline,
        }),
        z.object({
          type: z.literal("gap"),
          start: datetime,
          end: datetime,
          timeline: timeline,
        }),
      ])
    ),
  })
  .describe("Document exported using `org-memento-export-to-json` command.");

yargs
  .usage("Usage: $0 <cmd> [args]")
  .command(
    "validate",
    "Validate a document",
    (y) => {
      return y
        .option("file", {
          type: "string",
          describe: "File to validate",
          demandOption: true,
        })
        .alias("f", "file");
    },
    (argv) => {
      const file: string = argv.file;
      fs.readFile(file, "utf-8", (err, raw) => {
        if (err) {
          console.error(err);
          return;
        }
        const data = JSON.parse(raw);
        const result = document.safeParse(data);
        if (!result.success) {
          // @ts-ignore
          console.error(result.error.issues);
        }
      });
    }
  )
  .command(
    "schema",
    "Print the JSON schema",
    (y) => {
      return y;
    },
    (_) => {
      console.log(
        JSON.stringify(
          zodToJsonSchema(document, {
            $refStrategy: "relative",
          })
        )
      );
    }
  )
  .parse();
