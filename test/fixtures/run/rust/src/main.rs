fn twice(x: i32) -> i32 {
    x * 2
}

fn main() {
    println!("twice 21 is {}", twice(21));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn doubles() {
        assert_eq!(twice(2), 4);
    }

    #[test]
    fn fails_on_purpose() {
        assert_eq!(twice(2), 5);
    }
}
