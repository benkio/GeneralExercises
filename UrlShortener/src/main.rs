use rand::Rng;
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

fn random_lowercase(max_length: usize) -> String {
    let mut rng = rand::thread_rng();
    let mut result: String = String::new();
    for _ in 0..max_length {
        let random_index: usize = rng.gen_range(0..26);
        let selected_char: char = ASCII_LOWER[random_index];
        result.push(selected_char);
    }
    result
}


struct UrlShortener {
    url_database: HashMap<String, String>,
}


impl UrlShortener {

    fn new() -> Self {
        Self {
            url_database: HashMap::new(),
        }
    }


    fn shorten(&self, long_url: &str) -> String {
        let max_length = 13 - URL_PREFIX.len();
        if max_length <= 0 {
            panic!("max_length is: {} and cannot be <= 0", max_length);
        }
        let prefix: String = URL_PREFIX.to_string().clone();
        let postfix: String = random_lowercase(max_length);
        prefix + &postfix
    }

    // fn redirect(&self, short_url: &str) -> String {
    //     todo!()
    // }
}

fn main() {
    let url_shortener = UrlShortener::new();
    let test = url_shortener.shorten("www.benkio.github.io/");
    println!("{}", test);
}
