// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use hyper::{body::HttpBody, client::HttpConnector};

use tokio_native_tls::native_tls;

pub use hyper;
pub use hyper_tls;
pub use tokio_native_tls;

pub fn create_https_client<TBody>(
    tls_cert: Option<native_tls::Certificate>,
) -> Result<
    hyper::Client<hyper_tls::HttpsConnector<hyper::client::HttpConnector>, TBody>,
    native_tls::Error,
>
where
    TBody: HttpBody + Send + 'static,
    TBody::Data: Send,
    TBody::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    let tls = create_tls_connector(tls_cert).map(tokio_native_tls::TlsConnector::from)?;
    let mut http = HttpConnector::new();
    http.enforce_http(false);
    let https = hyper_tls::HttpsConnector::from((http, tls));
    Ok(hyper::Client::builder().build(https))
}

pub fn create_tls_connector(
    tls_cert: Option<native_tls::Certificate>,
) -> Result<native_tls::TlsConnector, native_tls::Error> {
    let mut tls = hyper_tls::native_tls::TlsConnector::builder();
    if let Some(cert) = tls_cert {
        tls.add_root_certificate(cert);
    }
    tls.build()
}
