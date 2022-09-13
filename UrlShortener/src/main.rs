use std::collections::HashMap;

static URL_PREFIX: &str = "short.ly/";
static ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e',
    'f', 'g', 'h', 'i', 'j',
    'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y',
    'z',
];

fn generate_short_link_postfix(generated_links: usize) -> String {
    let mut remainder: usize = generated_links;
    let mut modulo: usize    = remainder % 26;
    let mut div: usize       = remainder / 26;
    let mut result: String   = ASCII_LOWER[modulo].to_string();
    loop {
        if div == 0 {
            return result;
        } else {
            remainder = div - 1;
            modulo = remainder % 26;
            div = remainder / 26;
            let mut temp = ASCII_LOWER[modulo].to_string();
            temp.push_str(&result);
            result = temp;
        }
    }
}
struct UrlShortener {
    url_database_sl: HashMap<String, String>,
    url_database_ls: HashMap<String, String>,
    generated_links: usize,
}

impl UrlShortener {

    fn new() -> Self {
        Self {
            url_database_sl: HashMap::new(),
            url_database_ls: HashMap::new(),
            generated_links: 0,
        }
    }


    fn shorten(&mut self, long_url: &str) -> String {
        match self.url_database_ls.get(long_url) {
            Some(short_url) => short_url.to_string(),
            None => {
                let prefix: String = URL_PREFIX.to_string().clone();
                let postfix: String = generate_short_link_postfix(self.generated_links);
                let short_url: String = prefix + &postfix;
                self.generated_links = self.generated_links + 1;
                self.url_database_sl.insert(short_url.clone(), long_url.to_string());
                self.url_database_ls.insert(long_url.to_string(), short_url.clone());
                short_url
            }
        }
    }

    fn redirect(&self, short_url: &str) -> String {
        self.url_database_sl[short_url].to_string()
    }
}

fn main() {
    let mut url_shortener = UrlShortener::new();
    let test = url_shortener.shorten("www.benkio.github.io/");
    println!("simple test: {}", test);
    for i in 0..1000 {
        let result: String = generate_short_link_postfix(i);
        println!("{}: {}", i, result);
    }
}

#[cfg(test)]
mod tests {
    use super::UrlShortener;
    use crate::assert_valid_short_url;
    use crate::generate_short_link_postfix;

    #[test]
    fn generate_short_link_postfix_test() {
        let mut result: String = generate_short_link_postfix(0);
        assert_eq!(result, "a");

        result = generate_short_link_postfix(25);
        assert_eq!(result, "z");

        result = generate_short_link_postfix(26);
        assert_eq!(result, "aa");

        result = generate_short_link_postfix(27);
        assert_eq!(result, "ab");

        result = generate_short_link_postfix(702);
        assert_eq!(result, "aaa");
    }

    #[test]
    fn two_different_urls() {
        let mut url_shortener = UrlShortener::new();

        let short_url_1 = url_shortener.shorten("https://www.codewars.com/kata/5ef9ca8b76be6d001d5e1c3e");
        assert_valid_short_url!(&short_url_1);

        let short_url_2 = url_shortener.shorten("https://www.codewars.com/kata/5efae11e2d12df00331f91a6");
        assert_valid_short_url!(&short_url_2);

        assert_eq!(url_shortener.redirect(&short_url_1), "https://www.codewars.com/kata/5ef9ca8b76be6d001d5e1c3e");
        assert_eq!(url_shortener.redirect(&short_url_2), "https://www.codewars.com/kata/5efae11e2d12df00331f91a6");
    }

    #[test]
    fn same_urls() {
        let mut url_shortener = UrlShortener::new();

        let short_url_1 = url_shortener.shorten("https://www.codewars.com/kata/5ef9c85dc41b4e000f9a645f");
        assert_valid_short_url!(&short_url_1);

        let short_url_2 = url_shortener.shorten("https://www.codewars.com/kata/5ef9c85dc41b4e000f9a645f");
        assert_valid_short_url!(&short_url_2);

        assert_eq!(short_url_1, short_url_2, "Should work with the same long URLs");
        assert_eq!(
            url_shortener.redirect(&short_url_1),
            "https://www.codewars.com/kata/5ef9c85dc41b4e000f9a645f",
            "{} should redirect to https://www.codewars.com/kata/5ef9c85dc41b4e000f9a645f",
            &short_url_1,
        );
    }

    #[macro_export]
    macro_rules! assert_valid_short_url {
        ($url:expr) => {
            assert!(
                $url.starts_with("short.ly/"),
                "URL format is incorrect: should start with \"short.ly/\", got: {}",
                $url,
            );

            assert!(
                $url.len() < 14,
                "URL format is incorrect: length should be < 14 characters, got: {}",
                $url,
            );

            // As the URL contains "short.ly/", we can safely index using [9..]
            assert!(
                $url[9..].bytes().all(|b| b.is_ascii_lowercase()),
                "URL format is incorrect: should contain lowercase letters at the end, got: {}",
                $url,
            );
        }
    }
}
