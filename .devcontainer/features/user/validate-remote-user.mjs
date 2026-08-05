import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

// Runs vendored alongside devcontainer-feature.json — reads its own
// schema default from there instead of duplicating it, so this stays
// correct if the default ever changes.
const here = path.dirname(fileURLToPath(import.meta.url));

// config: the project's resolved devcontainer.json (parsed JSON object).
export default function validateRemoteUser(config) {
  const featureJson = JSON.parse(readFileSync(path.join(here, "devcontainer-feature.json"), "utf8"));
  const options = config.features?.["./features/user"] ?? {};
  const username = options.username ?? featureJson.options?.username?.default;

  const pass = config.remoteUser === username;
  return {
    pass,
    message: pass
      ? undefined
      : `devcontainer.json remoteUser (${JSON.stringify(config.remoteUser)}) doesn't match ` +
        `the user feature's username option (${JSON.stringify(username)})`,
  };
}
