# Security Hardening Guide

This document provides comprehensive security hardening guidelines for the Factory de Agentes platform, covering infrastructure, application, and operational security.

## Table of Contents

1. [Overview](#overview)
2. [CI/CD Security Scanning](#cicd-security-scanning)
3. [Kubernetes Security](#kubernetes-security)
4. [Secrets Management](#secrets-management)
5. [Web Application Firewall (WAF)](#web-application-firewall-waf)
6. [Network Security](#network-security)
7. [Container Security](#container-security)
8. [Operational Security](#operational-security)

---

## Overview

The Factory de Agentes security infrastructure implements defense-in-depth with multiple security layers:

```
                                    Internet
                                        |
                                   [WAF/NGINX]
                                        |
                              [Kubernetes Ingress]
                                        |
                    +-------------------+-------------------+
                    |                   |                   |
              [Network Policy]   [Network Policy]   [Network Policy]
                    |                   |                   |
               [API Pods]         [Worker Pods]      [Database Pods]
                    |                   |                   |
               [Pod Security]    [Pod Security]     [Pod Security]
                    |                   |                   |
              [Vault Secrets]   [Vault Secrets]    [Vault Secrets]
```

### Security Issues Addressed

| Issue | Title | Status |
|-------|-------|--------|
| #92 | Vulnerability Scanning in Docker Images | Implemented |
| #93 | Kubernetes Pod Security Standards (PSS) | Implemented |
| #94 | External Secrets Management (Vault) | Implemented |
| #99 | WAF (Web Application Firewall) | Implemented |

---

## CI/CD Security Scanning

### Configuration

The security scanning workflow is defined in `.github/workflows/security-scan.yml`.

### Scanners Included

#### 1. Trivy - Container Vulnerability Scanner

Scans Docker images for vulnerabilities in OS packages and application dependencies.

```yaml
- name: Run Trivy vulnerability scanner
  uses: aquasecurity/trivy-action@master
  with:
    image-ref: 'factory:scan'
    format: 'sarif'
    severity: 'CRITICAL,HIGH'
    exit-code: '1'
```

**Configuration Options:**
- `severity`: CRITICAL, HIGH, MEDIUM, LOW
- `exit-code`: 1 to fail on findings, 0 to only report
- `ignore-unfixed`: Skip vulnerabilities without fixes

#### 2. Snyk - Dependency Vulnerability Scanner

Scans Python dependencies for known vulnerabilities.

```yaml
- name: Run Snyk to check for vulnerabilities
  uses: snyk/actions/python@master
  env:
    SNYK_TOKEN: ${{ secrets.SNYK_TOKEN }}
  with:
    args: --severity-threshold=high
```

**Required Secret:** `SNYK_TOKEN`

#### 3. Gitleaks - Secret Scanner

Detects hardcoded secrets, API keys, and credentials in the codebase.

```yaml
- name: Run Gitleaks
  uses: gitleaks/gitleaks-action@v2
  env:
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

#### 4. Semgrep - Static Application Security Testing (SAST)

Analyzes code for security vulnerabilities using pattern matching.

```yaml
- name: Run Semgrep
  run: semgrep ci --sarif --output=semgrep-results.sarif
```

### Viewing Results

All scan results are uploaded to GitHub Security tab as SARIF files:
- Navigate to: Repository > Security > Code scanning alerts

### Scheduled Scans

Security scans run:
- On every push to `main` and `develop`
- On every pull request
- Daily at 2 AM UTC (scheduled scan)

---

## Kubernetes Security

### Pod Security Standards (PSS)

Kubernetes 1.25+ uses Pod Security Admission (PSA) instead of PodSecurityPolicy.

#### Namespace Labels

Apply security labels to the factory namespace:

```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: factory
  labels:
    pod-security.kubernetes.io/enforce: restricted
    pod-security.kubernetes.io/warn: restricted
    pod-security.kubernetes.io/audit: restricted
```

#### Pod Security Context

All pods must include:

```yaml
securityContext:
  runAsNonRoot: true
  runAsUser: 1000
  runAsGroup: 1000
  fsGroup: 1000
  seccompProfile:
    type: RuntimeDefault

containers:
  - name: app
    securityContext:
      allowPrivilegeEscalation: false
      readOnlyRootFilesystem: true
      capabilities:
        drop:
          - ALL
```

### Network Policies

Network policies implement zero-trust networking:

```
Factory Namespace Network Flow:

    [Ingress Controller]
           |
           v
    [API Pods:8000] <-----> [Dashboard:9001]
           |
     +-----+-----+
     |           |
     v           v
[Redis:6379]  [PostgreSQL:5432]
     ^           ^
     |           |
     +-----+-----+
           |
    [Worker Pods:8001]
```

#### Default Deny Policy

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny-all
  namespace: factory
spec:
  podSelector: {}
  policyTypes:
    - Ingress
    - Egress
```

This blocks all traffic by default. Specific policies then allow required communication.

### Resource Limits

LimitRange enforces resource constraints:

```yaml
limits:
  - type: Container
    default:
      memory: "512Mi"
      cpu: "500m"
    max:
      memory: "4Gi"
      cpu: "2"
```

### Configuration Files

| File | Purpose |
|------|---------|
| `k8s/pod-security-policy.yaml` | PSP, PSA labels, LimitRange, ResourceQuota |
| `k8s/network-policy.yaml` | Network isolation policies |

---

## Secrets Management

### HashiCorp Vault Integration

The `factory/core/secrets.py` module provides secure secrets management.

### Authentication Methods

#### 1. Token-Based Authentication (Development)

```python
from factory.core.secrets import VaultSecrets

vault = VaultSecrets(
    addr='https://vault.example.com:8200',
    token='hvs.xxxxx'
)

db_creds = vault.get_database_credentials()
```

#### 2. Kubernetes Service Account Authentication (Production)

```python
from factory.core.secrets import KubernetesVaultAuth

# Automatically uses SA token mounted in pod
vault = KubernetesVaultAuth(role='factory-api')

api_keys = vault.get_api_keys()
```

### Environment Variables Fallback

For local development without Vault:

```bash
export FACTORY_DATABASE_HOST=localhost
export FACTORY_DATABASE_PORT=5432
export FACTORY_DATABASE_USER=factory
export FACTORY_DATABASE_PASSWORD=secret
export FACTORY_API_KEYS_ANTHROPIC_KEY=sk-ant-xxxxx
```

### SecretsManager Usage

```python
from factory.core.secrets import get_secrets, get_database_url, get_api_key

# Get secrets manager instance
secrets = get_secrets()

# Get specific secrets
db_url = get_database_url()
api_key = get_api_key()
jwt_secret = secrets.get_jwt_secret()
```

### Vault Configuration

#### Required Vault Paths

```
secret/factory/database    # Database credentials
secret/factory/api-keys    # API keys (Anthropic, etc.)
secret/factory/auth        # JWT secrets
secret/factory/redis       # Redis credentials
```

#### Vault Policy

```hcl
path "secret/data/factory/*" {
  capabilities = ["read", "list"]
}

path "secret/metadata/factory/*" {
  capabilities = ["read", "list"]
}
```

#### Kubernetes Auth Role

```bash
vault write auth/kubernetes/role/factory-api \
    bound_service_account_names=factory-api \
    bound_service_account_namespaces=factory \
    policies=factory-secrets \
    ttl=1h
```

---

## Web Application Firewall (WAF)

### ModSecurity with NGINX

Configuration files in `nginx/` directory:
- `modsecurity.conf` - ModSecurity rules
- `nginx-modsecurity.conf` - NGINX configuration

### Protection Layers

#### 1. OWASP Core Rule Set (CRS)

Industry-standard ruleset protecting against:
- SQL Injection (SQLi)
- Cross-Site Scripting (XSS)
- Local/Remote File Inclusion (LFI/RFI)
- Remote Code Execution (RCE)
- Protocol violations

#### 2. Custom Factory Rules

| Rule ID | Protection |
|---------|------------|
| 1001-1002 | Directory traversal |
| 1010-1012 | SQL injection |
| 1020-1023 | XSS attacks |
| 1030-1031 | Command injection |
| 1040-1041 | Malicious file uploads |
| 1050-1051 | HTTP protocol attacks |
| 1060-1061 | Rate limiting |

### Rate Limiting

```nginx
# API endpoints: 10 requests/second with burst of 20
limit_req zone=api_limit burst=20 nodelay;

# Auth endpoints: 5 requests/minute with burst of 5
limit_req zone=auth_limit burst=5 nodelay;
```

### Security Headers

All responses include:

```
X-Frame-Options: SAMEORIGIN
X-Content-Type-Options: nosniff
X-XSS-Protection: 1; mode=block
Referrer-Policy: strict-origin-when-cross-origin
Content-Security-Policy: default-src 'self'
Strict-Transport-Security: max-age=31536000; includeSubDomains
```

### Deployment

#### Docker Compose

```yaml
services:
  nginx-waf:
    image: owasp/modsecurity-crs:nginx
    volumes:
      - ./nginx/modsecurity.conf:/etc/nginx/modsecurity.conf
      - ./nginx/nginx-modsecurity.conf:/etc/nginx/nginx.conf
    ports:
      - "443:443"
```

#### Kubernetes Ingress

Use NGINX Ingress Controller with ModSecurity:

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  annotations:
    nginx.ingress.kubernetes.io/enable-modsecurity: "true"
    nginx.ingress.kubernetes.io/modsecurity-snippet: |
      SecRuleEngine On
```

---

## Network Security

### TLS/SSL Configuration

#### Minimum TLS Version

```nginx
ssl_protocols TLSv1.2 TLSv1.3;
```

#### Strong Cipher Suites

```nginx
ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256;
ssl_prefer_server_ciphers off;
```

### Certificate Management

Use cert-manager for automatic certificate renewal:

```yaml
apiVersion: cert-manager.io/v1
kind: Certificate
metadata:
  name: factory-tls
spec:
  secretName: factory-tls
  issuerRef:
    name: letsencrypt-prod
    kind: ClusterIssuer
  dnsNames:
    - factory.example.com
```

---

## Container Security

### Dockerfile Best Practices

```dockerfile
# Use specific version, not latest
FROM python:3.11-slim-bookworm

# Run as non-root user
RUN useradd -m -u 1000 factory
USER factory

# Use multi-stage builds
FROM builder AS runtime

# No shell for reduced attack surface
RUN rm /bin/sh

# Read-only filesystem
VOLUME ["/tmp"]
```

### Image Scanning

Before deployment, images are scanned for:
- Known CVEs in base image
- Vulnerable dependencies
- Hardcoded secrets
- Misconfigurations

---

## Operational Security

### Audit Logging

All security events are logged:

```nginx
SecAuditEngine RelevantOnly
SecAuditLogRelevantStatus "^(?:5|4(?!04))"
SecAuditLog /var/log/nginx/modsec_audit.log
```

### Monitoring Alerts

Configure alerts for:
- Blocked requests exceeding threshold
- Authentication failures
- Anomaly score spikes
- Rate limit triggers

### Incident Response

1. **Detection**: Security events trigger alerts
2. **Analysis**: Review audit logs and SARIF reports
3. **Containment**: Network policies isolate affected components
4. **Recovery**: Rollback to known-good state
5. **Post-mortem**: Update security rules

### Regular Security Tasks

| Task | Frequency |
|------|-----------|
| Review security scan results | Daily |
| Update dependencies | Weekly |
| Rotate secrets | Monthly |
| Security audit | Quarterly |
| Penetration testing | Annually |

---

## Quick Start Checklist

### Initial Setup

- [ ] Configure GitHub secrets for security scanners
  - `SNYK_TOKEN`
  - `SEMGREP_APP_TOKEN` (optional)
  - `GITLEAKS_LICENSE` (optional)

- [ ] Deploy Vault and configure factory secrets
  ```bash
  vault kv put secret/factory/database host=... user=... password=...
  vault kv put secret/factory/api-keys anthropic_key=...
  ```

- [ ] Apply Kubernetes security policies
  ```bash
  kubectl apply -f k8s/pod-security-policy.yaml
  kubectl apply -f k8s/network-policy.yaml
  ```

- [ ] Deploy WAF
  ```bash
  docker-compose -f docker-compose.security.yml up -d nginx-waf
  ```

### Verification

```bash
# Test WAF is blocking attacks
curl -X GET "https://factory.example.com/api/?id=1' OR '1'='1"
# Expected: 403 Forbidden

# Verify pod security
kubectl auth can-i --as=system:serviceaccount:factory:default \
  create pods --subresource=exec -n factory
# Expected: no

# Check network policies
kubectl get networkpolicies -n factory
```

---

## References

- [OWASP ModSecurity Core Rule Set](https://coreruleset.org/)
- [Kubernetes Pod Security Standards](https://kubernetes.io/docs/concepts/security/pod-security-standards/)
- [HashiCorp Vault Documentation](https://developer.hashicorp.com/vault/docs)
- [Trivy Documentation](https://aquasecurity.github.io/trivy/)
- [Snyk Documentation](https://docs.snyk.io/)

---

*Last updated: December 2024*
*Factory de Agentes - Security Hardening Guide*
