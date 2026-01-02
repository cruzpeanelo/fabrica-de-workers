/**
 * Carousel Component - Issue #429
 * Carrossel de imagens e cards
 */

import React, { useState, useRef, useCallback, useEffect } from 'react';
import {
  View,
  Text,
  FlatList,
  Image,
  TouchableOpacity,
  StyleSheet,
  Dimensions,
  ViewStyle,
  Animated,
  NativeScrollEvent,
  NativeSyntheticEvent,
} from 'react-native';
import { ChevronLeft, ChevronRight } from 'lucide-react-native';
import { useTheme } from '../context/ThemeContext';

const { width: SCREEN_WIDTH } = Dimensions.get('window');

type CarouselItem = {
  id: string;
  image?: string;
  title?: string;
  subtitle?: string;
  content?: React.ReactNode;
};

interface CarouselProps {
  data: CarouselItem[];
  renderItem?: (item: CarouselItem, index: number) => React.ReactNode;
  itemWidth?: number;
  itemHeight?: number;
  gap?: number;
  showPagination?: boolean;
  showArrows?: boolean;
  autoPlay?: boolean;
  autoPlayInterval?: number;
  loop?: boolean;
  onIndexChange?: (index: number) => void;
  onItemPress?: (item: CarouselItem, index: number) => void;
  style?: ViewStyle;
}

export function Carousel({
  data,
  renderItem,
  itemWidth = SCREEN_WIDTH - 48,
  itemHeight = 200,
  gap = 16,
  showPagination = true,
  showArrows = false,
  autoPlay = false,
  autoPlayInterval = 3000,
  loop = false,
  onIndexChange,
  onItemPress,
  style,
}: CarouselProps) {
  const { colors } = useTheme();
  const [currentIndex, setCurrentIndex] = useState(0);
  const flatListRef = useRef<FlatList>(null);
  const autoPlayRef = useRef<NodeJS.Timeout>();

  const totalWidth = itemWidth + gap;

  // Auto play
  useEffect(() => {
    if (autoPlay && data.length > 1) {
      autoPlayRef.current = setInterval(() => {
        const nextIndex = loop
          ? (currentIndex + 1) % data.length
          : Math.min(currentIndex + 1, data.length - 1);

        if (nextIndex !== currentIndex || loop) {
          scrollToIndex(nextIndex);
        }
      }, autoPlayInterval);

      return () => {
        if (autoPlayRef.current) {
          clearInterval(autoPlayRef.current);
        }
      };
    }
  }, [autoPlay, autoPlayInterval, currentIndex, data.length, loop]);

  const scrollToIndex = useCallback((index: number) => {
    flatListRef.current?.scrollToOffset({
      offset: index * totalWidth,
      animated: true,
    });
  }, [totalWidth]);

  const handleScroll = useCallback((event: NativeSyntheticEvent<NativeScrollEvent>) => {
    const offsetX = event.nativeEvent.contentOffset.x;
    const index = Math.round(offsetX / totalWidth);

    if (index !== currentIndex && index >= 0 && index < data.length) {
      setCurrentIndex(index);
      onIndexChange?.(index);
    }
  }, [currentIndex, data.length, onIndexChange, totalWidth]);

  const goToPrevious = () => {
    const prevIndex = loop
      ? (currentIndex - 1 + data.length) % data.length
      : Math.max(currentIndex - 1, 0);
    scrollToIndex(prevIndex);
  };

  const goToNext = () => {
    const nextIndex = loop
      ? (currentIndex + 1) % data.length
      : Math.min(currentIndex + 1, data.length - 1);
    scrollToIndex(nextIndex);
  };

  const defaultRenderItem = (item: CarouselItem, index: number) => (
    <TouchableOpacity
      activeOpacity={onItemPress ? 0.8 : 1}
      onPress={() => onItemPress?.(item, index)}
      style={[
        styles.defaultItem,
        {
          width: itemWidth,
          height: itemHeight,
          backgroundColor: colors.surface,
        },
      ]}
    >
      {item.image && (
        <Image
          source={{ uri: item.image }}
          style={styles.itemImage}
          resizeMode="cover"
        />
      )}
      {(item.title || item.subtitle) && (
        <View style={styles.itemContent}>
          {item.title && (
            <Text style={[styles.itemTitle, { color: '#FFF' }]} numberOfLines={1}>
              {item.title}
            </Text>
          )}
          {item.subtitle && (
            <Text style={[styles.itemSubtitle, { color: 'rgba(255,255,255,0.8)' }]} numberOfLines={2}>
              {item.subtitle}
            </Text>
          )}
        </View>
      )}
      {item.content}
    </TouchableOpacity>
  );

  return (
    <View style={style}>
      <View style={styles.carouselContainer}>
        {/* Left Arrow */}
        {showArrows && (
          <TouchableOpacity
            style={[
              styles.arrowButton,
              styles.arrowLeft,
              { backgroundColor: colors.surface },
            ]}
            onPress={goToPrevious}
            disabled={!loop && currentIndex === 0}
          >
            <ChevronLeft
              color={!loop && currentIndex === 0 ? colors.border : colors.text}
              size={24}
            />
          </TouchableOpacity>
        )}

        {/* Carousel */}
        <FlatList
          ref={flatListRef}
          data={data}
          horizontal
          pagingEnabled={false}
          snapToInterval={totalWidth}
          decelerationRate="fast"
          showsHorizontalScrollIndicator={false}
          onScroll={handleScroll}
          scrollEventThrottle={16}
          contentContainerStyle={{
            paddingHorizontal: (SCREEN_WIDTH - itemWidth) / 2,
          }}
          renderItem={({ item, index }) => (
            <View style={{ marginRight: index < data.length - 1 ? gap : 0 }}>
              {renderItem ? renderItem(item, index) : defaultRenderItem(item, index)}
            </View>
          )}
          keyExtractor={(item) => item.id}
          getItemLayout={(_, index) => ({
            length: totalWidth,
            offset: totalWidth * index,
            index,
          })}
        />

        {/* Right Arrow */}
        {showArrows && (
          <TouchableOpacity
            style={[
              styles.arrowButton,
              styles.arrowRight,
              { backgroundColor: colors.surface },
            ]}
            onPress={goToNext}
            disabled={!loop && currentIndex === data.length - 1}
          >
            <ChevronRight
              color={!loop && currentIndex === data.length - 1 ? colors.border : colors.text}
              size={24}
            />
          </TouchableOpacity>
        )}
      </View>

      {/* Pagination */}
      {showPagination && data.length > 1 && (
        <View style={styles.pagination}>
          {data.map((_, index) => (
            <TouchableOpacity
              key={index}
              onPress={() => scrollToIndex(index)}
              style={[
                styles.paginationDot,
                {
                  backgroundColor:
                    index === currentIndex ? colors.primary : colors.border,
                  width: index === currentIndex ? 24 : 8,
                },
              ]}
            />
          ))}
        </View>
      )}
    </View>
  );
}

// Banner Carousel - Fullwidth image carousel
interface BannerCarouselProps {
  images: { id: string; uri: string; title?: string }[];
  height?: number;
  autoPlay?: boolean;
  onPress?: (image: { id: string; uri: string }, index: number) => void;
  style?: ViewStyle;
}

export function BannerCarousel({
  images,
  height = 180,
  autoPlay = true,
  onPress,
  style,
}: BannerCarouselProps) {
  const { colors } = useTheme();
  const [currentIndex, setCurrentIndex] = useState(0);
  const flatListRef = useRef<FlatList>(null);
  const autoPlayRef = useRef<NodeJS.Timeout>();

  useEffect(() => {
    if (autoPlay && images.length > 1) {
      autoPlayRef.current = setInterval(() => {
        const nextIndex = (currentIndex + 1) % images.length;
        flatListRef.current?.scrollToOffset({
          offset: nextIndex * SCREEN_WIDTH,
          animated: true,
        });
        setCurrentIndex(nextIndex);
      }, 4000);

      return () => {
        if (autoPlayRef.current) {
          clearInterval(autoPlayRef.current);
        }
      };
    }
  }, [autoPlay, currentIndex, images.length]);

  const handleScroll = (event: NativeSyntheticEvent<NativeScrollEvent>) => {
    const index = Math.round(event.nativeEvent.contentOffset.x / SCREEN_WIDTH);
    if (index !== currentIndex) {
      setCurrentIndex(index);
    }
  };

  return (
    <View style={style}>
      <FlatList
        ref={flatListRef}
        data={images}
        horizontal
        pagingEnabled
        showsHorizontalScrollIndicator={false}
        onScroll={handleScroll}
        scrollEventThrottle={16}
        renderItem={({ item, index }) => (
          <TouchableOpacity
            activeOpacity={onPress ? 0.9 : 1}
            onPress={() => onPress?.(item, index)}
            style={{ width: SCREEN_WIDTH }}
          >
            <Image
              source={{ uri: item.uri }}
              style={{ width: SCREEN_WIDTH, height }}
              resizeMode="cover"
            />
            {item.title && (
              <View style={styles.bannerOverlay}>
                <Text style={styles.bannerTitle}>{item.title}</Text>
              </View>
            )}
          </TouchableOpacity>
        )}
        keyExtractor={(item) => item.id}
      />

      {/* Pagination dots */}
      {images.length > 1 && (
        <View style={styles.bannerPagination}>
          {images.map((_, index) => (
            <View
              key={index}
              style={[
                styles.bannerDot,
                {
                  backgroundColor: index === currentIndex ? '#FFF' : 'rgba(255,255,255,0.5)',
                },
              ]}
            />
          ))}
        </View>
      )}
    </View>
  );
}

// Card Carousel - Horizontal scrolling cards
interface CardCarouselProps {
  title?: string;
  data: any[];
  renderCard: (item: any, index: number) => React.ReactNode;
  cardWidth?: number;
  gap?: number;
  onSeeAll?: () => void;
  style?: ViewStyle;
}

export function CardCarousel({
  title,
  data,
  renderCard,
  cardWidth = 280,
  gap = 12,
  onSeeAll,
  style,
}: CardCarouselProps) {
  const { colors } = useTheme();

  return (
    <View style={style}>
      {title && (
        <View style={styles.cardCarouselHeader}>
          <Text style={[styles.cardCarouselTitle, { color: colors.text }]}>
            {title}
          </Text>
          {onSeeAll && (
            <TouchableOpacity onPress={onSeeAll}>
              <Text style={[styles.seeAllText, { color: colors.primary }]}>
                Ver todos
              </Text>
            </TouchableOpacity>
          )}
        </View>
      )}

      <FlatList
        data={data}
        horizontal
        showsHorizontalScrollIndicator={false}
        contentContainerStyle={{ paddingHorizontal: 16 }}
        renderItem={({ item, index }) => (
          <View style={{ width: cardWidth, marginRight: gap }}>
            {renderCard(item, index)}
          </View>
        )}
        keyExtractor={(_, index) => `card-${index}`}
      />
    </View>
  );
}

const styles = StyleSheet.create({
  carouselContainer: {
    position: 'relative',
  },
  defaultItem: {
    borderRadius: 12,
    overflow: 'hidden',
  },
  itemImage: {
    width: '100%',
    height: '100%',
    position: 'absolute',
  },
  itemContent: {
    position: 'absolute',
    bottom: 0,
    left: 0,
    right: 0,
    padding: 16,
    backgroundColor: 'rgba(0,0,0,0.4)',
  },
  itemTitle: {
    fontSize: 18,
    fontWeight: '600',
  },
  itemSubtitle: {
    fontSize: 14,
    marginTop: 4,
  },
  arrowButton: {
    position: 'absolute',
    top: '50%',
    marginTop: -20,
    width: 40,
    height: 40,
    borderRadius: 20,
    justifyContent: 'center',
    alignItems: 'center',
    zIndex: 10,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.1,
    shadowRadius: 4,
    elevation: 3,
  },
  arrowLeft: {
    left: 8,
  },
  arrowRight: {
    right: 8,
  },
  pagination: {
    flexDirection: 'row',
    justifyContent: 'center',
    alignItems: 'center',
    marginTop: 16,
    gap: 6,
  },
  paginationDot: {
    height: 8,
    borderRadius: 4,
  },
  bannerOverlay: {
    position: 'absolute',
    bottom: 0,
    left: 0,
    right: 0,
    padding: 16,
    backgroundColor: 'rgba(0,0,0,0.3)',
  },
  bannerTitle: {
    color: '#FFF',
    fontSize: 16,
    fontWeight: '600',
  },
  bannerPagination: {
    position: 'absolute',
    bottom: 12,
    left: 0,
    right: 0,
    flexDirection: 'row',
    justifyContent: 'center',
    gap: 6,
  },
  bannerDot: {
    width: 8,
    height: 8,
    borderRadius: 4,
  },
  cardCarouselHeader: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    paddingHorizontal: 16,
    marginBottom: 12,
  },
  cardCarouselTitle: {
    fontSize: 18,
    fontWeight: '600',
  },
  seeAllText: {
    fontSize: 14,
    fontWeight: '500',
  },
});
