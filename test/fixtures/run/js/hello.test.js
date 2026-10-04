const test = require("node:test");
const assert = require("node:assert");

const twice = (x) => x * 2;

test("doubles", () => {
  assert.strictEqual(twice(2), 4);
});

test("fails on purpose", () => {
  assert.strictEqual(twice(2), 5);
});
