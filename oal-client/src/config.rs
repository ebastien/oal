use anyhow::Ok;
use clap::Parser as ClapParser;
use oal_model::locator::Locator;
use serde::Deserialize;
use std::path::{Path, PathBuf};

/// Compiles a program into an OpenAPI description in YAML.
#[derive(ClapParser, Debug)]
struct Args {
    /// The relative URL to the main program
    #[clap(short = 'm', long = "main")]
    main: Option<String>,

    /// The relative URL to the target OpenAPI description
    #[clap(short = 't', long = "target")]
    target: Option<String>,

    /// The relative URL to a base OpenAPI description
    #[clap(short = 'b', long = "base")]
    base: Option<String>,

    /// The path to the configuration file
    #[clap(short = 'c', long = "conf", parse(from_os_str))]
    config: Option<PathBuf>,

    /// The relative URL to the target documentation
    #[clap(short = 'd', long = "doc")]
    doc: Option<String>,
}

#[derive(Deserialize, Default, Debug)]
struct File {
    api: Api,
}

#[derive(Deserialize, Default, Debug)]
struct Api {
    main: Option<String>,
    target: Option<String>,
    base: Option<String>,
}

#[derive(Debug)]
pub struct Config {
    args: Args,
    file: File,
    root: Locator,
}

impl Config {
    pub fn new(cfg: Option<&Path>) -> anyhow::Result<Self> {
        let args: Args = Args::parse();

        let config = cfg.or(args.config.as_deref());

        let (root, file) = if let Some(path) = config {
            let root = Locator::try_from(path)?;
            let cfg = std::fs::read_to_string(path)?;
            let file = toml::from_str::<File>(&cfg)?;
            (root, file)
        } else {
            let cwd = std::env::current_dir()?;
            let loc = Locator::try_from(cwd.as_path())?;
            let root = loc.as_base();
            let file = File::default();
            (root, file)
        };

        Ok(Config { args, file, root })
    }

    pub fn main(&self) -> anyhow::Result<Locator> {
        match self.args.main.as_ref().or(self.file.api.main.as_ref()) {
            Some(p) => Ok(self.root.join(p)?),
            None => Err(anyhow::Error::msg("main module not specified")),
        }
    }

    pub fn target(&self) -> anyhow::Result<Locator> {
        match self.args.target.as_ref().or(self.file.api.target.as_ref()) {
            Some(p) => Ok(self.root.join(p)?),
            None => Err(anyhow::Error::msg("target not specified")),
        }
    }

    pub fn base(&self) -> anyhow::Result<Option<Locator>> {
        match self.args.base.as_ref().or(self.file.api.base.as_ref()) {
            Some(p) => Ok(Some(self.root.join(p)?)),
            None => Ok(None),
        }
    }

    pub fn doc(&self) -> anyhow::Result<Option<Locator>> {
        match self.args.doc.as_ref() {
            Some(p) => Ok(Some(self.root.join(p)?)),
            None => Ok(None),
        }
    }
}
