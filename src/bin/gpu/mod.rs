use graphics::Canvas;
use graphics::Rect;
use graphics::Pixel;
use graphics::Point;

pub struct Gpu {
    ptr: *const u8,
    width: usize,
    height: usize,
}

impl Gpu {
    pub fn new(width: usize, height: usize, ptr: *const u8) -> Self {
        Gpu { ptr, width, height }
    }

    pub fn fill_canvas(&self, buffer: &mut [u32], canvas: &Canvas, rect: Option<Rect>) {
        let rect = rect.unwrap_or(canvas.into());
        let inner = canvas.centered_rect(self.height, self.width);
        for r in rect.y..(rect.y + rect.height) {
            for c in rect.x..(rect.x + rect.width) {
                let point = (c, r).into();
                let color = if !inner.contain(point) {
                    canvas.bg
                } else {
                    let relative = point.relative_to(inner.origin());

                    if self.get(relative) {
                        canvas.fg
                    } else {
                        canvas.bg
                    }
                };
                buffer[r * canvas.width + c] = Pixel::from(color).into();
            }
        }
    }

    fn mem(&self, pos: usize) -> u8 {
        unsafe { *self.ptr.offset(pos as isize) }
    }

    fn get(&self, point: Point) -> bool {
        let (x, y) = ((self.width - point.y), point.x);
        let pos = x / 8 + y * self.width / 8;
        let bit = x % 8;
        let data = self.mem(pos);

        data & (0x01 << bit) != 0
    }
}
