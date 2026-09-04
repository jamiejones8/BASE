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
    "BASE_PITCHER_STUFF_MODEL_FILE",
    "BASE_PITCHER_LOCATION_MODEL_FILE",
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


if __name__ == "__main__":
    unittest.main()
