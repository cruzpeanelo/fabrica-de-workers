{{/*
=============================================================================
Fabrica de Agentes - Helm Template Helpers
=============================================================================
Funcoes auxiliares para templates Helm
=============================================================================
*/}}

{{/*
Expand the name of the chart.
*/}}
{{- define "fabrica-agentes.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
*/}}
{{- define "fabrica-agentes.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "fabrica-agentes.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "fabrica-agentes.labels" -}}
helm.sh/chart: {{ include "fabrica-agentes.chart" . }}
{{ include "fabrica-agentes.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "fabrica-agentes.selectorLabels" -}}
app.kubernetes.io/name: {{ include "fabrica-agentes.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
API labels
*/}}
{{- define "fabrica-agentes.api.labels" -}}
{{ include "fabrica-agentes.labels" . }}
app.kubernetes.io/component: api
{{- end }}

{{/*
API selector labels
*/}}
{{- define "fabrica-agentes.api.selectorLabels" -}}
{{ include "fabrica-agentes.selectorLabels" . }}
app.kubernetes.io/component: api
{{- end }}

{{/*
Worker labels
*/}}
{{- define "fabrica-agentes.worker.labels" -}}
{{ include "fabrica-agentes.labels" . }}
app.kubernetes.io/component: worker
{{- end }}

{{/*
Worker selector labels
*/}}
{{- define "fabrica-agentes.worker.selectorLabels" -}}
{{ include "fabrica-agentes.selectorLabels" . }}
app.kubernetes.io/component: worker
{{- end }}

{{/*
Watcher labels
*/}}
{{- define "fabrica-agentes.watcher.labels" -}}
{{ include "fabrica-agentes.labels" . }}
app.kubernetes.io/component: watcher
{{- end }}

{{/*
Watcher selector labels
*/}}
{{- define "fabrica-agentes.watcher.selectorLabels" -}}
{{ include "fabrica-agentes.selectorLabels" . }}
app.kubernetes.io/component: watcher
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "fabrica-agentes.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "fabrica-agentes.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
Database URL
*/}}
{{- define "fabrica-agentes.databaseUrl" -}}
{{- if .Values.postgresql.enabled }}
postgresql://{{ .Values.postgresql.auth.username }}:{{ .Values.postgresql.auth.password }}@{{ include "fabrica-agentes.fullname" . }}-postgresql:5432/{{ .Values.postgresql.auth.database }}
{{- else if .Values.externalDatabase.enabled }}
postgresql://{{ .Values.secrets.databaseUser }}:{{ .Values.secrets.databasePassword }}@{{ .Values.externalDatabase.host }}:{{ .Values.externalDatabase.port }}/{{ .Values.externalDatabase.database }}
{{- else }}
sqlite:///app/factory/database/factory.db
{{- end }}
{{- end }}

{{/*
Redis URL
*/}}
{{- define "fabrica-agentes.redisUrl" -}}
{{- if .Values.redis.enabled }}
redis://{{ include "fabrica-agentes.fullname" . }}-redis-master:6379/0
{{- else if .Values.externalRedis.enabled }}
redis://{{ .Values.externalRedis.host }}:{{ .Values.externalRedis.port }}/{{ .Values.externalRedis.db }}
{{- else }}
redis://localhost:6379/0
{{- end }}
{{- end }}

{{/*
Image pull secrets
*/}}
{{- define "fabrica-agentes.imagePullSecrets" -}}
{{- if .Values.global.imagePullSecrets }}
imagePullSecrets:
{{- range .Values.global.imagePullSecrets }}
  - name: {{ .name }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Full image name
*/}}
{{- define "fabrica-agentes.image" -}}
{{- $registry := .Values.global.imageRegistry | default "" -}}
{{- $repository := .repository -}}
{{- $tag := .tag | default "latest" -}}
{{- if $registry }}
{{- printf "%s/%s:%s" $registry $repository $tag }}
{{- else }}
{{- printf "%s:%s" $repository $tag }}
{{- end }}
{{- end }}
