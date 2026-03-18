import type { Plugin } from "@opencode-ai/plugin";

export const NotifierPlugin: Plugin = async ({ $ }) => {
  return {
    event: async ({ event }) => {
      const type = event.type;
      const data = (event as any).data;

      try {
        if (type === "session.idle") {
          await $`notify-gui 'is now idle' '[OpenCode]'`;
        } else if (type === "permission.asked") {
          const tool = data?.tool?.name || "a tool";
          await $`notify-gui 'needs permissions (for ${tool})' '[OpenCode]'`;
        } else if (type === "session.created") {
          await $`notify-gui 'Session started' '[OpenCode]'`;
        } else if (type === "session.error") {
          await $`notify-gui 'encountered an error' '[OpenCode]'`;
        }
      } catch (e) {
        // Ignore notification errors
      }
    },
  };
};

export default NotifierPlugin;
