#!/usr/bin/env python3
"""Keep build-time model deferral narrow and runtime validation strict."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
RUNTIME_MODELS = (
    "BASE_BREWSTUFF_MODEL_FILE",
    "BASE_SCOUT_MODELS_FILE",
    "BASE_PITCHER_LOCATION_MODEL_FILE",
)
VALDR_ARCHIVE = (
    "https://cran.r-project.org/src/contrib/Archive/valdr/"
    "valdr_3.0.0.tar.gz"
)


class DeploymentChecks(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="base-deploy-check-")
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name)
        self.env = os.environ.copy()
        for name in RUNTIME_MODELS:
            self.env[name] = str(self.path / name)

    def check(self, *args, success):
        result = subprocess.run(
            ["Rscript", "scripts/checks/check_team_config.R", *args],
            cwd=ROOT, env=self.env, capture_output=True, text=True,
        )
        output = result.stdout + result.stderr
        self.assertEqual(result.returncode == 0, success, output)
        return output

    def test_build_defers_missing_runtime_models(self):
        output = self.check("--build", success=True)
        self.assertIn("deferred until container startup", output)

    def test_runtime_rejects_missing_models_with_resolved_paths(self):
        output = self.check(success=False)
        self.assertIn("Configured model file(s) not found", output)
        for name in RUNTIME_MODELS:
            self.assertIn(self.env[name], output)

    def test_build_still_requires_bundled_xwoba_grid(self):
        self.env["BASE_XWGRID_FILE"] = str(self.path / "absent-grid.rds")
        output = self.check("--build", success=False)
        self.assertIn("xwoba_grid_file", output)

    def test_build_rejects_present_but_unresolved_model_pointer(self):
        pointer = self.path / "pointer.model"
        pointer.write_text("version https://git-lfs.github.com/spec/v1\n")
        self.env["BASE_BREWSTUFF_MODEL_FILE"] = str(pointer)
        output = self.check("--build", success=False)
        self.assertIn("unresolved Git LFS pointers: brewstuff_model_file", output)

    def test_unknown_mode_cannot_disable_checks(self):
        output = self.check("--skip-models", success=False)
        self.assertIn("Usage:", output)

    def test_valdr_bypasses_the_predating_package_snapshot(self):
        for filename in ("Dockerfile", "Dockerfile.dependencies"):
            source = (ROOT / filename).read_text(encoding="utf-8")
            install2_block = source.split("RUN install2.r --error", 1)[1].split("\nRUN ", 1)[0]
            install2_packages = "\n".join(
                line for line in install2_block.splitlines()
                if not line.lstrip().startswith("#")
            )
            self.assertNotIn("valdr", install2_packages, filename)
            self.assertIn(VALDR_ARCHIVE, source, filename)

    def test_player_health_runtime_inputs_enter_the_image(self):
        dockerignore = (ROOT / ".dockerignore").read_text(encoding="utf-8")
        excluded = {
            "Sports Science 2/data/",
            "Sports Science 2/2026 Fall Roster Template.xlsx",
        }
        for item in excluded:
            self.assertNotIn(item, dockerignore)

        deployment_config = (
            ROOT / "R" / "config" / "player_health_deployment.R"
        ).read_text(encoding="utf-8")
        for name in (
            "VALD_CLIENT_ID",
            "VALD_CLIENT_SECRET",
            "VALD_TEAM_ID",
            "VALD_REGION",
            "VALD_AUTO_REFRESH",
            "VALD_REFRESH_HOURS",
        ):
            self.assertRegex(deployment_config, rf'{name}\s*=\s*"[^"]+"')

        integration = (
            ROOT / "R" / "integrations" / "player_health_workspace.R"
        ).read_text(encoding="utf-8")
        self.assertIn('base_source("R/config/player_health_deployment.R"', integration)
        self.assertIn('"/base-data/app_state/player-health"', integration)

    def test_player_health_checks_valdr_not_its_internal_keyring_import(self):
        integration = (
            ROOT / "R" / "integrations" / "player_health_workspace.R"
        ).read_text(encoding="utf-8")
        required_block = integration.split(
            "BASE_PLAYER_HEALTH_REQUIRED_PACKAGES <- c(", 1
        )[1].split("\n)", 1)[0]
        self.assertIn('"valdr"', required_block)
        self.assertNotIn('"keyring"', required_block)

        # valdr imports keyring, so it remains installed in both the current
        # image and the reusable dependency image. It just is not a separate
        # app-level startup gate.
        for filename in ("Dockerfile", "Dockerfile.dependencies"):
            source = (ROOT / filename).read_text(encoding="utf-8")
            self.assertIn("keyring", source, filename)


if __name__ == "__main__":
    unittest.main()
