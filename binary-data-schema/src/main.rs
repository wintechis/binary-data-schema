use anyhow::Result;
use binary_data_schema::{DataSchema, Decoder, Encoder};
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
enum Command {
    /// Encode JSON input into hex-encoded data.
    Encode,
    /// Decode hex-encoded data into JSON.
    Decode,
}

/// Given a schema the input is either en- or decoded.
#[derive(Debug, StructOpt)]
struct Cli {
    /// What to do.
    #[structopt(subcommand)]
    command: Command,
    /// Path to the schema in JSON.
    #[structopt(short, long)]
    schema: PathBuf,
    /// Path to the input either JSON to encode or hex-encoded text to decode.
    #[structopt(short, long)]
    input: PathBuf,
}

fn main() -> Result<()> {
    let cli = Cli::from_args();
    let schema = File::open(cli.schema)?;
    let schema = BufReader::new(schema);
    let schema: DataSchema = serde_json::from_reader(schema)?;

    let mut input = File::open(cli.input)?;
    let mut buffer = String::new();
    input.read_to_string(&mut buffer)?;

    match cli.command {
        Command::Decode => {
            let input = hex::decode(buffer)?;
            let mut input = std::io::Cursor::new(input);
            let value = schema.decode(&mut input)?;
            println!("{}", serde_json::to_string_pretty(&value)?);
        }
        Command::Encode => {
            let value = serde_json::from_str(&buffer)?;
            let mut buffer = Vec::new();
            schema.encode(&mut buffer, &value)?;
            println!("{}", hex::encode(buffer));
        }
    }

    Ok(())
}
