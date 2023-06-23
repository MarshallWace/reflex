// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use hyper::body::HttpBody;
use hyper_rustls::ConfigBuilderExt;

pub use hyper;
pub use hyper_rustls;
pub use rustls;

pub fn create_https_client<TBody>(
    ca_certs: Option<Vec<rustls::Certificate>>,
) -> Result<
    hyper::Client<hyper_rustls::HttpsConnector<hyper::client::HttpConnector>, TBody>,
    rustls::Error,
>
where
    TBody: HttpBody + Send + 'static,
    TBody::Data: Send,
    TBody::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    let tls_config = rustls::ClientConfig::builder().with_safe_defaults();
    let tls_config = if let Some(tls_certs) = ca_certs {
        let roots = tls_certs.iter().fold(
            Result::<_, rustls::Error>::Ok(rustls::RootCertStore::empty()),
            |result, tls_cert| {
                let mut roots = result?;
                roots.add(tls_cert)?;
                Ok(roots)
            },
        );
        roots.map(|roots| tls_config.with_root_certificates(roots))
    } else {
        Ok(tls_config.with_native_roots())
    }?
    .with_no_client_auth();
    let connector = hyper_rustls::HttpsConnectorBuilder::default()
        .with_tls_config(tls_config)
        .https_or_http()
        .enable_http1()
        .build();
    Ok(hyper::Client::builder().build(connector))
}

pub fn parse_ca_certs(pem_bytes: &[u8]) -> Result<Vec<rustls::Certificate>, std::io::Error> {
    rustls_pemfile::certs(&mut std::io::BufReader::new(pem_bytes))
        .map(|certs| certs.into_iter().map(rustls::Certificate).collect())
}
