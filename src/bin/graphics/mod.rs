#[derive(Copy, Clone)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

pub const BLACK: Color = Color { red: 0x00, green: 0x00, blue: 0x00 };
pub const WHITE: Color = Color { red: 0xFF, green: 0xFF, blue: 0xFF };
pub const RED: Color = Color { red: 0xFF, green: 0x00, blue: 0x00 };
pub const GREEN: Color = Color { red: 0x00, green: 0xFF, blue: 0x00 };
pub const BLUE: Color = Color { red: 0x00, green: 0x00, blue: 0xFF };

const ALPHA_FILL: u8 = 255;

const BLUE_OFFSET: usize = 0;
const GREEN_OFFSET: usize = 8;
const RED_OFFSET: usize = 16;
const ALPHA_OFFSET: usize = 24;

#[derive(Default, Copy, Clone)]
pub struct Pixel(u32);

impl Pixel {
    pub fn by_color(red: u8, green: u8, blue: u8) -> Self {
        Self::by(red, green, blue, ALPHA_FILL)
    }

    pub fn by(red: u8, green: u8, blue: u8, alpha: u8) -> Self {
        Pixel(((blue as u32) << BLUE_OFFSET) | ((green as u32) << GREEN_OFFSET) |
            ((red as u32) << RED_OFFSET) | ((alpha as u32) << ALPHA_OFFSET))
    }

    pub fn bw(level: u8) -> Self {
        Self::bw_alpha(level, ALPHA_FILL)
    }

    pub fn bw_alpha(level: u8, alpha: u8) -> Self {
        Pixel::by(level, level, level, alpha)
    }

    pub fn as_bw(&self) -> Self {
        let level = (((self.blue() as u32) + (self.green() as u32) + (self.red() as u32)) / 3) as u8;
        Self::bw_alpha(level, self.alpha())
    }

    pub fn blue(&self) -> u8 {
        ((self.0 >> BLUE_OFFSET) & 0xFF) as u8
    }

    pub fn green(&self) -> u8 {
        ((self.0 >> GREEN_OFFSET) & 0xFF) as u8
    }

    pub fn red(&self) -> u8 {
        ((self.0 >> RED_OFFSET) & 0xFF) as u8
    }

    pub fn alpha(&self) -> u8 {
        ((self.0 >> ALPHA_OFFSET) & 0xFF) as u8
    }
}

impl From<u32> for Pixel {
    fn from(val: u32) -> Self {
        Pixel(val)
    }
}

impl<'a> From<&'a [u8]> for Pixel {
    fn from(v: &'a [u8]) -> Self {
        Pixel::by(v[0], v[1], v[2], v[3])
    }
}

impl Into<u32> for Pixel {
    fn into(self) -> u32 {
        self.0
    }
}

impl From<Color> for Pixel {
    fn from(c: Color) -> Self {
        Pixel::by_color(c.red, c.green, c.blue)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Point {
    pub x: usize,
    pub y: usize,
}

impl Point {
    pub fn relative_to(self, other: Self) -> Self {
        Point { x: self.x - other.x, y: self.y - other.y }
    }
}

impl From<(usize, usize)> for Point {
    fn from(d: (usize, usize)) -> Self {
        Point { x: d.0, y: d.1 }
    }
}

impl Into<(usize, usize)> for Point {
    fn into(self) -> (usize, usize) {
        (self.x, self.y)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Rect {
    pub x: usize,
    pub y: usize,
    pub width: usize,
    pub height: usize,
}

impl Rect {
    pub fn contain(&self, point: Point) -> bool {
        !(point.x < self.x || point.y < self.y || point.x >= (self.x + self.width) || point.y >= (self.y + self.height))
    }

    pub fn origin(&self) -> Point {
        Point { x: self.x, y: self.y }
    }

    pub fn split_horizontal(&self, h: usize) -> (Rect, Rect) {
        assert!(h <= self.height);
        (Rect {
            x: self.x,
            y: self.y,
            width: self.width,
            height: h,
        }, Rect {
            x: self.x,
            y: self.y + h,
            width: self.width,
            height: self.height - h,
        })
    }

    pub fn split_vertical(&self, w: usize) -> (Rect, Rect) {
        assert!(w <= self.width);
        (Rect {
            x: self.x,
            y: self.y,
            width: w,
            height: self.height,
        }, Rect {
            x: self.x + w,
            y: self.y,
            width: self.width - w,
            height: self.height,
        })
    }
}

pub struct Canvas {
    pub width: usize,
    pub height: usize,
    pub fg: Color,
    pub bg: Color,
}

impl Canvas {
    pub fn centered_rect(&self, width: usize, height: usize) -> Rect {
        assert!(width <= self.width);
        assert!(height <= self.height);

        let left_margin = (self.width - width) / 2;
        let up_margin = (self.height - height) / 2;

        return Rect {
            x: left_margin,
            y: up_margin,
            width,
            height,
        };
    }
}

impl<'a> From<&'a Canvas> for Rect {
    fn from(canvas: &'a Canvas) -> Self {
        Rect {
            x: 0,
            y: 0,
            width: canvas.width,
            height: canvas.height,
        }
    }
}
