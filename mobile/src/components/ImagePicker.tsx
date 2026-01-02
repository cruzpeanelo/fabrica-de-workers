/**
 * ImagePicker Component - Issue #429
 * Selecao e visualizacao de imagens
 */

import React, { useState } from 'react';
import {
  View,
  Text,
  TouchableOpacity,
  Image,
  Modal,
  StyleSheet,
  FlatList,
  Dimensions,
  ViewStyle,
  ImageStyle,
} from 'react-native';
import {
  Camera,
  Image as ImageIcon,
  X,
  ZoomIn,
  ZoomOut,
  Trash2,
  Plus,
} from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

const { width: SCREEN_WIDTH, height: SCREEN_HEIGHT } = Dimensions.get('window');

interface ImageAsset {
  uri: string;
  width?: number;
  height?: number;
  type?: string;
  name?: string;
}

// Image Picker
interface ImagePickerProps {
  value?: ImageAsset | ImageAsset[];
  onChange: (images: ImageAsset | ImageAsset[]) => void;
  multiple?: boolean;
  maxImages?: number;
  placeholder?: string;
  label?: string;
  showPreview?: boolean;
  previewSize?: number;
  onCamera?: () => void;
  onGallery?: () => void;
  disabled?: boolean;
  error?: string;
  style?: ViewStyle;
}

export function ImagePicker({
  value,
  onChange,
  multiple = false,
  maxImages = 10,
  placeholder = 'Selecionar imagem',
  label,
  showPreview = true,
  previewSize = 80,
  onCamera,
  onGallery,
  disabled = false,
  error,
  style,
}: ImagePickerProps) {
  const { colors } = useTheme();
  const [showOptions, setShowOptions] = useState(false);
  const [viewerImage, setViewerImage] = useState<ImageAsset | null>(null);

  const images = Array.isArray(value) ? value : value ? [value] : [];
  const canAddMore = images.length < maxImages;

  const handleSelectSource = (source: 'camera' | 'gallery') => {
    setShowOptions(false);
    if (source === 'camera' && onCamera) {
      onCamera();
    } else if (source === 'gallery' && onGallery) {
      onGallery();
    }
  };

  const handleRemoveImage = (index: number) => {
    const newImages = images.filter((_, i) => i !== index);
    if (multiple) {
      onChange(newImages);
    } else {
      onChange(newImages[0] || null as any);
    }
  };

  const renderImagePreview = (image: ImageAsset, index: number) => (
    <View key={index} style={styles.previewContainer}>
      <TouchableOpacity
        onPress={() => setViewerImage(image)}
        activeOpacity={0.8}
      >
        <Image
          source={{ uri: image.uri }}
          style={[
            styles.previewImage,
            { width: previewSize, height: previewSize },
          ]}
        />
      </TouchableOpacity>
      {!disabled && (
        <TouchableOpacity
          style={[styles.removeButton, { backgroundColor: colors.error }]}
          onPress={() => handleRemoveImage(index)}
        >
          <X color="#FFF" size={12} />
        </TouchableOpacity>
      )}
    </View>
  );

  return (
    <View style={style}>
      {label && (
        <Text style={[styles.label, { color: colors.text }]}>{label}</Text>
      )}

      {/* Image previews */}
      {showPreview && images.length > 0 && (
        <View style={styles.previewsRow}>
          {images.map((img, i) => renderImagePreview(img, i))}
          {multiple && canAddMore && !disabled && (
            <TouchableOpacity
              style={[
                styles.addMoreButton,
                {
                  width: previewSize,
                  height: previewSize,
                  borderColor: colors.border,
                },
              ]}
              onPress={() => setShowOptions(true)}
            >
              <Plus color={colors.textSecondary} size={24} />
            </TouchableOpacity>
          )}
        </View>
      )}

      {/* Picker trigger */}
      {(!showPreview || images.length === 0) && (
        <TouchableOpacity
          style={[
            styles.trigger,
            {
              backgroundColor: colors.surface,
              borderColor: error ? colors.error : colors.border,
            },
          ]}
          onPress={() => !disabled && setShowOptions(true)}
          disabled={disabled || (!multiple && images.length > 0)}
          activeOpacity={0.7}
        >
          <ImageIcon color={colors.textSecondary} size={24} />
          <Text style={[styles.triggerText, { color: colors.textSecondary }]}>
            {placeholder}
          </Text>
          {multiple && (
            <Text style={[styles.counterText, { color: colors.textSecondary }]}>
              {images.length}/{maxImages}
            </Text>
          )}
        </TouchableOpacity>
      )}

      {error && (
        <Text style={[styles.error, { color: colors.error }]}>{error}</Text>
      )}

      {/* Source selection modal */}
      <Modal visible={showOptions} transparent animationType="fade">
        <TouchableOpacity
          style={styles.overlay}
          activeOpacity={1}
          onPress={() => setShowOptions(false)}
        >
          <View style={[styles.optionsModal, { backgroundColor: colors.surface }]}>
            <Text style={[styles.optionsTitle, { color: colors.text }]}>
              Selecionar imagem
            </Text>

            <TouchableOpacity
              style={[styles.optionButton, { backgroundColor: colors.background }]}
              onPress={() => handleSelectSource('camera')}
            >
              <Camera color={colors.primary} size={24} />
              <Text style={[styles.optionText, { color: colors.text }]}>
                Tirar foto
              </Text>
            </TouchableOpacity>

            <TouchableOpacity
              style={[styles.optionButton, { backgroundColor: colors.background }]}
              onPress={() => handleSelectSource('gallery')}
            >
              <ImageIcon color={colors.primary} size={24} />
              <Text style={[styles.optionText, { color: colors.text }]}>
                Escolher da galeria
              </Text>
            </TouchableOpacity>

            <TouchableOpacity
              style={[styles.cancelButton, { borderColor: colors.border }]}
              onPress={() => setShowOptions(false)}
            >
              <Text style={{ color: colors.text }}>Cancelar</Text>
            </TouchableOpacity>
          </View>
        </TouchableOpacity>
      </Modal>

      {/* Image viewer */}
      {viewerImage && (
        <ImageViewer
          images={images}
          initialIndex={images.findIndex((img) => img.uri === viewerImage.uri)}
          onClose={() => setViewerImage(null)}
        />
      )}
    </View>
  );
}

// Image Viewer - Full screen
interface ImageViewerProps {
  images: ImageAsset[];
  initialIndex?: number;
  onClose: () => void;
  onDelete?: (index: number) => void;
}

export function ImageViewer({
  images,
  initialIndex = 0,
  onClose,
  onDelete,
}: ImageViewerProps) {
  const { colors } = useTheme();
  const [currentIndex, setCurrentIndex] = useState(initialIndex);
  const [scale, setScale] = useState(1);

  const handleZoomIn = () => {
    setScale((prev) => Math.min(prev + 0.5, 3));
  };

  const handleZoomOut = () => {
    setScale((prev) => Math.max(prev - 0.5, 0.5));
  };

  const handleDelete = () => {
    if (onDelete) {
      onDelete(currentIndex);
      if (currentIndex >= images.length - 1 && currentIndex > 0) {
        setCurrentIndex(currentIndex - 1);
      }
      if (images.length === 1) {
        onClose();
      }
    }
  };

  return (
    <Modal visible transparent animationType="fade">
      <View style={[styles.viewerContainer, { backgroundColor: '#000' }]}>
        {/* Header */}
        <View style={styles.viewerHeader}>
          <TouchableOpacity onPress={onClose} style={styles.viewerButton}>
            <X color="#FFF" size={24} />
          </TouchableOpacity>
          <Text style={styles.viewerCounter}>
            {currentIndex + 1} / {images.length}
          </Text>
          <View style={styles.viewerActions}>
            <TouchableOpacity onPress={handleZoomOut} style={styles.viewerButton}>
              <ZoomOut color="#FFF" size={24} />
            </TouchableOpacity>
            <TouchableOpacity onPress={handleZoomIn} style={styles.viewerButton}>
              <ZoomIn color="#FFF" size={24} />
            </TouchableOpacity>
            {onDelete && (
              <TouchableOpacity onPress={handleDelete} style={styles.viewerButton}>
                <Trash2 color="#FFF" size={24} />
              </TouchableOpacity>
            )}
          </View>
        </View>

        {/* Images */}
        <FlatList
          data={images}
          horizontal
          pagingEnabled
          showsHorizontalScrollIndicator={false}
          initialScrollIndex={initialIndex}
          getItemLayout={(_, index) => ({
            length: SCREEN_WIDTH,
            offset: SCREEN_WIDTH * index,
            index,
          })}
          onMomentumScrollEnd={(e) => {
            const index = Math.round(e.nativeEvent.contentOffset.x / SCREEN_WIDTH);
            setCurrentIndex(index);
            setScale(1);
          }}
          renderItem={({ item }) => (
            <View style={styles.viewerImageContainer}>
              <Image
                source={{ uri: item.uri }}
                style={[
                  styles.viewerImage,
                  { transform: [{ scale }] },
                ]}
                resizeMode="contain"
              />
            </View>
          )}
          keyExtractor={(item, index) => `${item.uri}-${index}`}
        />

        {/* Thumbnails */}
        {images.length > 1 && (
          <View style={styles.thumbnailsContainer}>
            <FlatList
              data={images}
              horizontal
              showsHorizontalScrollIndicator={false}
              contentContainerStyle={styles.thumbnailsList}
              renderItem={({ item, index }) => (
                <TouchableOpacity
                  style={[
                    styles.thumbnail,
                    currentIndex === index && styles.thumbnailActive,
                  ]}
                  onPress={() => setCurrentIndex(index)}
                >
                  <Image source={{ uri: item.uri }} style={styles.thumbnailImage} />
                </TouchableOpacity>
              )}
              keyExtractor={(item, index) => `thumb-${index}`}
            />
          </View>
        )}
      </View>
    </Modal>
  );
}

// Avatar Picker - Circular image picker for profile
interface AvatarPickerProps {
  value?: string;
  onChange: (uri: string) => void;
  size?: number;
  placeholder?: React.ReactNode;
  onCamera?: () => void;
  onGallery?: () => void;
  disabled?: boolean;
  style?: ViewStyle;
}

export function AvatarPicker({
  value,
  onChange,
  size = 100,
  placeholder,
  onCamera,
  onGallery,
  disabled = false,
  style,
}: AvatarPickerProps) {
  const { colors } = useTheme();
  const [showOptions, setShowOptions] = useState(false);

  const handleSelectSource = (source: 'camera' | 'gallery') => {
    setShowOptions(false);
    if (source === 'camera' && onCamera) {
      onCamera();
    } else if (source === 'gallery' && onGallery) {
      onGallery();
    }
  };

  return (
    <View style={[styles.avatarContainer, style]}>
      <TouchableOpacity
        style={[
          styles.avatarTouchable,
          {
            width: size,
            height: size,
            borderRadius: size / 2,
            backgroundColor: colors.border,
          },
        ]}
        onPress={() => !disabled && setShowOptions(true)}
        disabled={disabled}
        activeOpacity={0.8}
      >
        {value ? (
          <Image
            source={{ uri: value }}
            style={[
              styles.avatarImage,
              { width: size, height: size, borderRadius: size / 2 },
            ]}
          />
        ) : (
          placeholder || <Camera color={colors.textSecondary} size={size * 0.4} />
        )}
      </TouchableOpacity>

      <TouchableOpacity
        style={[
          styles.avatarEditButton,
          { backgroundColor: colors.primary },
          { right: 0, bottom: 0 },
        ]}
        onPress={() => !disabled && setShowOptions(true)}
        disabled={disabled}
      >
        <Camera color="#FFF" size={14} />
      </TouchableOpacity>

      {/* Source selection modal */}
      <Modal visible={showOptions} transparent animationType="fade">
        <TouchableOpacity
          style={styles.overlay}
          activeOpacity={1}
          onPress={() => setShowOptions(false)}
        >
          <View style={[styles.optionsModal, { backgroundColor: colors.surface }]}>
            <Text style={[styles.optionsTitle, { color: colors.text }]}>
              Foto de perfil
            </Text>

            <TouchableOpacity
              style={[styles.optionButton, { backgroundColor: colors.background }]}
              onPress={() => handleSelectSource('camera')}
            >
              <Camera color={colors.primary} size={24} />
              <Text style={[styles.optionText, { color: colors.text }]}>
                Tirar foto
              </Text>
            </TouchableOpacity>

            <TouchableOpacity
              style={[styles.optionButton, { backgroundColor: colors.background }]}
              onPress={() => handleSelectSource('gallery')}
            >
              <ImageIcon color={colors.primary} size={24} />
              <Text style={[styles.optionText, { color: colors.text }]}>
                Escolher da galeria
              </Text>
            </TouchableOpacity>

            {value && (
              <TouchableOpacity
                style={[styles.optionButton, { backgroundColor: colors.background }]}
                onPress={() => {
                  onChange('');
                  setShowOptions(false);
                }}
              >
                <Trash2 color={colors.error} size={24} />
                <Text style={[styles.optionText, { color: colors.error }]}>
                  Remover foto
                </Text>
              </TouchableOpacity>
            )}

            <TouchableOpacity
              style={[styles.cancelButton, { borderColor: colors.border }]}
              onPress={() => setShowOptions(false)}
            >
              <Text style={{ color: colors.text }}>Cancelar</Text>
            </TouchableOpacity>
          </View>
        </TouchableOpacity>
      </Modal>
    </View>
  );
}

const styles = StyleSheet.create({
  label: {
    fontSize: 14,
    fontWeight: '500',
    marginBottom: 6,
  },
  trigger: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
    paddingVertical: 24,
    borderRadius: 12,
    borderWidth: 2,
    borderStyle: 'dashed',
    gap: 8,
  },
  triggerText: {
    fontSize: 15,
  },
  counterText: {
    fontSize: 13,
    marginLeft: 8,
  },
  error: {
    fontSize: 12,
    marginTop: 4,
  },
  previewsRow: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    gap: 8,
    marginBottom: 12,
  },
  previewContainer: {
    position: 'relative',
  },
  previewImage: {
    borderRadius: 8,
  },
  removeButton: {
    position: 'absolute',
    top: -6,
    right: -6,
    width: 20,
    height: 20,
    borderRadius: 10,
    justifyContent: 'center',
    alignItems: 'center',
  },
  addMoreButton: {
    borderRadius: 8,
    borderWidth: 2,
    borderStyle: 'dashed',
    justifyContent: 'center',
    alignItems: 'center',
  },
  overlay: {
    flex: 1,
    backgroundColor: 'rgba(0, 0, 0, 0.5)',
    justifyContent: 'flex-end',
  },
  optionsModal: {
    borderTopLeftRadius: 24,
    borderTopRightRadius: 24,
    padding: 24,
  },
  optionsTitle: {
    fontSize: 18,
    fontWeight: '600',
    textAlign: 'center',
    marginBottom: 24,
  },
  optionButton: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 16,
    borderRadius: 12,
    marginBottom: 12,
    gap: 12,
  },
  optionText: {
    fontSize: 16,
    fontWeight: '500',
  },
  cancelButton: {
    paddingVertical: 16,
    borderRadius: 12,
    alignItems: 'center',
    borderWidth: 1,
    marginTop: 8,
  },
  viewerContainer: {
    flex: 1,
  },
  viewerHeader: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    paddingHorizontal: 16,
    paddingTop: 48,
    paddingBottom: 16,
  },
  viewerButton: {
    padding: 8,
  },
  viewerCounter: {
    color: '#FFF',
    fontSize: 16,
    fontWeight: '600',
  },
  viewerActions: {
    flexDirection: 'row',
    gap: 8,
  },
  viewerImageContainer: {
    width: SCREEN_WIDTH,
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  viewerImage: {
    width: SCREEN_WIDTH,
    height: SCREEN_HEIGHT * 0.6,
  },
  thumbnailsContainer: {
    paddingVertical: 16,
  },
  thumbnailsList: {
    paddingHorizontal: 16,
    gap: 8,
  },
  thumbnail: {
    width: 60,
    height: 60,
    borderRadius: 8,
    overflow: 'hidden',
    opacity: 0.6,
    marginRight: 8,
  },
  thumbnailActive: {
    opacity: 1,
    borderWidth: 2,
    borderColor: '#FFF',
  },
  thumbnailImage: {
    width: '100%',
    height: '100%',
  },
  avatarContainer: {
    position: 'relative',
    alignSelf: 'center',
  },
  avatarTouchable: {
    justifyContent: 'center',
    alignItems: 'center',
    overflow: 'hidden',
  },
  avatarImage: {
    width: '100%',
    height: '100%',
  },
  avatarEditButton: {
    position: 'absolute',
    width: 28,
    height: 28,
    borderRadius: 14,
    justifyContent: 'center',
    alignItems: 'center',
    borderWidth: 2,
    borderColor: '#FFF',
  },
});
