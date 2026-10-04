import test from "node:test";
import assert from "node:assert";

const twice = (x: number): number => x * 2;

test("doubles", () => {
  assert.strictEqual(twice(2), 4);
});

test("fails on purpose", () => {
  assert.strictEqual(twice(2), 5);
});
