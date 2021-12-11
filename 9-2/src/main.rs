use std::fs;

fn main() {
    let filename = "input.txt";
    let heightmap = fs::read_to_string(filename)
        .map(to_heightmap)
        .expect("Couldn't read file");
    let low_point_positions = get_low_point_positions(&heightmap);
    let mut bassin_sizes: Vec<usize> = low_point_positions
        .into_iter()
        .map(|point| compute_bassin(&heightmap, None, point).expect("Could not calculate bassin for point"))
        .map(|bassin| bassin.len())
        .collect();


    bassin_sizes.sort();
    bassin_sizes.reverse();
    bassin_sizes.truncate(3);

    let result = bassin_sizes
        .into_iter()
        .reduce(|acc, v| acc * v)
        .expect("Too few values present in list to multiply");

    println!("Sum of 3 largest bassins: {}", result);
}

fn to_heightmap(input: String) -> Heightmap {
    let rows = input.split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).expect("Could not parse number"))
                .collect::<Vec<u32>>()

        })
    .collect();

    Heightmap{rows}
}

fn get_low_point_positions(heightmap: &Heightmap) -> Vec<Position> {
    let mut low_point_positions: Vec<(usize, usize)> = vec![];

    let max_i = heightmap.row_count();
    let max_j = heightmap.column_count();

    for i in 0..max_i {
        for j in 0..max_j {
            let position = (i,j);
            if heightmap.is_lowpoint(position) {
                low_point_positions.push(position);
            }
        }
    }

    low_point_positions
}

fn compute_bassin(heightmap: &Heightmap, previous_point: Option<Position>, current_point: Position) -> Option<Vec<Position>> {
    let current_point_value = heightmap.get_point_value(current_point);
    let neighbor_points = heightmap.get_neighbor_points(current_point);

    if current_point_value == 9 {
        return None
    }

    if let Some(point) = previous_point {
        let previous_point_value = heightmap.get_point_value(point);
        if current_point_value < previous_point_value {
            return None
        }
    }

    let mut bassin: Vec<Position> = neighbor_points
        .into_iter()
        .filter_map(|next_point| {
            match previous_point {
                Some(point) if point == next_point => None,
                _ => compute_bassin(heightmap, Some(current_point), next_point)
            }
        })
        .flatten()
        .collect();

    bassin.push(current_point);
    bassin.sort();
    bassin.dedup();

    Some(bassin)
}

type Position = (usize, usize);

struct Heightmap {
    rows: Vec<Vec<u32>>
}

impl Heightmap {
    fn row_count(&self) -> usize {
        self.rows.len()
    }

    fn column_count(&self) -> usize {
        self.rows[0].len()
    }

    fn get_point_value(&self, (i, j): Position) -> u32 {
        self.rows.get(i).and_then(|row| row.get(j)).map(|value| *value).expect("Position not in bounds of map")
    }

    fn get_neighbor_points(&self, (i, j): Position) -> Vec<Position> {
        let max_i = self.row_count() - 1;
        let max_j = self.column_count() - 1;

        let mut neighbor_points = Vec::new();

        if i > 0 {
            neighbor_points.push((i-1, j));
        }

        if i < max_i {
            neighbor_points.push((i+1, j));
        }

        if j > 0 {
            neighbor_points.push((i, j-1));
        }

        if j < max_j {
            neighbor_points.push((i, j+1));
        }

        neighbor_points
    }

    fn is_lowpoint(&self, position: Position) -> bool {
        let point_value = self.get_point_value(position);

        self.get_neighbor_points(position)
            .into_iter()
            .map(|p| self.get_point_value(p))
            .all(|p| p > point_value)
    }
}
