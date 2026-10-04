package main

import "testing"

func TestDoubles(t *testing.T) {
	if twice(2) != 4 {
		t.Errorf("twice(2) = %d", twice(2))
	}
}

func TestFailsOnPurpose(t *testing.T) {
	if twice(2) != 5 {
		t.Errorf("twice(2) = %d, not 5", twice(2))
	}
}
