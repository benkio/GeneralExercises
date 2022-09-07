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
    [3, 2, 1, 0].iter().fold((String::new(), generated_links), |acc, x| {
        let (mut result, reminder): (String, usize) = acc;
        let index_base: usize = 26_usize.pow(*x);
        let current_index: usize =  reminder / index_base;
        let mut result_str: String = String::new();
        if current_index > 0 {
            result_str.push(ASCII_LOWER[current_index - 1]);
            result.push_str(&result_str);
            (result, reminder % index_base)
        } else { (result, reminder) }
    }).0.chars().rev().collect::<String>()
}
struct UrlShortener {
    url_database: HashMap<String, String>,
    generated_links: usize,
}

impl UrlShortener {

    fn new() -> Self {
        Self {
            url_database: HashMap::new(),
            generated_links: 0,
        }
    }


    fn shorten(&mut self, long_url: &str) -> String {
        match self.url_database.iter().find(|&(_, v)| v == long_url) {
            Some((short_url, _)) => short_url.to_string(),
            None => {
                let prefix: String = URL_PREFIX.to_string().clone();
                self.generated_links = self.generated_links + 1;
                let postfix: String = generate_short_link_postfix(self.generated_links);
                let short_url: String = prefix + &postfix;
                self.url_database.insert(short_url.clone(), long_url.to_string());
                short_url
            }
        }
    }

    fn redirect(&self, short_url: &str) -> String {
        self.url_database[short_url].to_string()
    }
}

fn main() {
    let mut url_shortener = UrlShortener::new();
    let test = url_shortener.shorten("www.benkio.github.io/");
    println!("simple test: {}", test);
    let result: String = generate_short_link_postfix(0);
    println!("big number test: {}", result);
}

#[cfg(test)]
mod tests {
    use super::UrlShortener;
    use crate::assert_valid_short_url;

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
