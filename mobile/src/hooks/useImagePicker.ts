/**
 * useImagePicker Hook - Selecao e captura de imagens
 * Para anexar arquivos aos requisitos
 */

import { useState } from 'react';
import { Alert, Platform } from 'react-native';
import * as ImagePicker from 'expo-image-picker';
import * as FileSystem from 'expo-file-system';
import { uploadApi } from '../services/api';

interface ImageInfo {
  uri: string;
  width: number;
  height: number;
  type?: string;
  fileName?: string;
  fileSize?: number;
}

interface UseImagePickerOptions {
  maxWidth?: number;
  maxHeight?: number;
  quality?: number;
  allowsEditing?: boolean;
  onUploadProgress?: (progress: number) => void;
}

export function useImagePicker(options: UseImagePickerOptions = {}) {
  const {
    maxWidth = 1024,
    maxHeight = 1024,
    quality = 0.8,
    allowsEditing = true,
    onUploadProgress,
  } = options;

  const [selectedImage, setSelectedImage] = useState<ImageInfo | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [uploadedUrl, setUploadedUrl] = useState<string | null>(null);

  // Verificar e solicitar permissoes
  async function requestPermissions(type: 'camera' | 'library'): Promise<boolean> {
    if (type === 'camera') {
      const { status } = await ImagePicker.requestCameraPermissionsAsync();
      if (status !== 'granted') {
        Alert.alert(
          'Permissao Necessaria',
          'Precisamos de acesso a camera para tirar fotos.',
          [{ text: 'OK' }]
        );
        return false;
      }
    } else {
      const { status } = await ImagePicker.requestMediaLibraryPermissionsAsync();
      if (status !== 'granted') {
        Alert.alert(
          'Permissao Necessaria',
          'Precisamos de acesso a galeria para selecionar fotos.',
          [{ text: 'OK' }]
        );
        return false;
      }
    }
    return true;
  }

  // Abrir camera
  async function openCamera(): Promise<ImageInfo | null> {
    const hasPermission = await requestPermissions('camera');
    if (!hasPermission) return null;

    try {
      const result = await ImagePicker.launchCameraAsync({
        mediaTypes: ImagePicker.MediaTypeOptions.Images,
        allowsEditing,
        aspect: [4, 3],
        quality,
      });

      if (!result.canceled && result.assets[0]) {
        const asset = result.assets[0];
        const imageInfo: ImageInfo = {
          uri: asset.uri,
          width: asset.width,
          height: asset.height,
          type: asset.mimeType || 'image/jpeg',
          fileName: asset.fileName || `photo_${Date.now()}.jpg`,
          fileSize: asset.fileSize,
        };
        setSelectedImage(imageInfo);
        return imageInfo;
      }
      return null;
    } catch (error) {
      console.error('Erro ao abrir camera:', error);
      Alert.alert('Erro', 'Nao foi possivel abrir a camera.');
      return null;
    }
  }

  // Abrir galeria
  async function openGallery(): Promise<ImageInfo | null> {
    const hasPermission = await requestPermissions('library');
    if (!hasPermission) return null;

    try {
      const result = await ImagePicker.launchImageLibraryAsync({
        mediaTypes: ImagePicker.MediaTypeOptions.Images,
        allowsEditing,
        aspect: [4, 3],
        quality,
      });

      if (!result.canceled && result.assets[0]) {
        const asset = result.assets[0];
        const imageInfo: ImageInfo = {
          uri: asset.uri,
          width: asset.width,
          height: asset.height,
          type: asset.mimeType || 'image/jpeg',
          fileName: asset.fileName || `image_${Date.now()}.jpg`,
          fileSize: asset.fileSize,
        };
        setSelectedImage(imageInfo);
        return imageInfo;
      }
      return null;
    } catch (error) {
      console.error('Erro ao abrir galeria:', error);
      Alert.alert('Erro', 'Nao foi possivel abrir a galeria.');
      return null;
    }
  }

  // Mostrar opcoes (camera ou galeria)
  function showImageOptions(): Promise<ImageInfo | null> {
    return new Promise((resolve) => {
      Alert.alert(
        'Selecionar Imagem',
        'Escolha uma opcao',
        [
          {
            text: 'Camera',
            onPress: async () => {
              const result = await openCamera();
              resolve(result);
            },
          },
          {
            text: 'Galeria',
            onPress: async () => {
              const result = await openGallery();
              resolve(result);
            },
          },
          {
            text: 'Cancelar',
            style: 'cancel',
            onPress: () => resolve(null),
          },
        ]
      );
    });
  }

  // Upload da imagem
  async function uploadImage(image?: ImageInfo): Promise<string | null> {
    const imageToUpload = image || selectedImage;
    if (!imageToUpload) {
      Alert.alert('Erro', 'Nenhuma imagem selecionada.');
      return null;
    }

    setIsLoading(true);
    try {
      const url = await uploadApi.uploadImage(
        imageToUpload.uri,
        imageToUpload.fileName || `upload_${Date.now()}.jpg`
      );
      setUploadedUrl(url);
      return url;
    } catch (error) {
      console.error('Erro ao fazer upload:', error);
      Alert.alert('Erro', 'Nao foi possivel enviar a imagem.');
      return null;
    } finally {
      setIsLoading(false);
    }
  }

  // Limpar selecao
  function clearSelection() {
    setSelectedImage(null);
    setUploadedUrl(null);
  }

  // Obter tamanho do arquivo formatado
  function getFormattedFileSize(bytes?: number): string {
    if (!bytes) return 'Tamanho desconhecido';
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
    return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
  }

  return {
    selectedImage,
    uploadedUrl,
    isLoading,
    openCamera,
    openGallery,
    showImageOptions,
    uploadImage,
    clearSelection,
    getFormattedFileSize,
  };
}
