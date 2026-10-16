import { execFile } from "node:child_process";

// V2 plugin context, narrowed to what this plugin uses. The full type lives in
// `@opencode/plugin`, which is not resolvable from a local plugin here.
type Ctx = {
  event: {
    subscribe(options?: { signal?: AbortSignal }): AsyncIterable<{
      type: string;
      data?: Record<string, any>;
    }>;
  };
};

const notify = (message: string, title = "[OpenCode]") =>
  new Promise<void>((resolve) => {
    execFile("notify-gui", [message, title], () => resolve());
  });

export default {
  id: "notifier",
  setup(ctx: Ctx) {
    const controller = new AbortController();

    void (async () => {
      for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
        try {
          if (event.type === "session.idle") {
            await notify("is now idle");
          } else if (event.type === "permission.asked") {
            // V2 permission requests carry an action (shell, edit, ...), not a tool name.
            await notify(`needs permissions (for ${event.data?.action ?? "a tool"})`);
          } else if (event.type === "session.created") {
            await notify("Session started");
          } else if (event.type === "session.execution.failed") {
            // V1's `session.error` is legacy and not on the V2 public event stream.
            await notify("encountered an error");
          }
        } catch {
          // Ignore notification errors
        }
      }
    })().catch(() => {});

    return () => controller.abort();
  },
};
