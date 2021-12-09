use std::fs;

fn main() {
    let filename = "input.txt";
    let heightmap = fs::read_to_string(filename)
        .map(to_heightmap)
        .expect("Couldn't read file");

    let mut low_points: Vec<u32> = vec![];

    let max_i = heightmap.len();
    let max_j = heightmap.get(0).expect("Map is empty").len();

    for i in 0..max_i {
        for j in 0..max_j {
            let point = heightmap[i][j];
            let neighbor_points = get_neighbor_points(i, j, &heightmap);

            if is_lowpoint(point, neighbor_points) {
                low_points.push(point);
            }
        }
    }

    let low_points_sum: u32 = low_points.iter().fold(0, |acc, p| acc + p + 1);

    println!("Sum of all low points: {}", low_points_sum)

}

fn to_heightmap(input: String) -> Vec<Vec<u32>> {
    input.split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).expect("Could not parse number"))
                .collect::<Vec<u32>>()

        })
    .collect()
}

fn is_lowpoint(point: u32, neighbor_points: Vec<u32>) -> bool {
    neighbor_points.iter().all(|p| *p > point)
}

fn get_neighbor_points(i: usize, j: usize, heightmap: &Vec<Vec<u32>>) -> Vec<u32> {
    vec![
        if i > 0 { heightmap.get(i-1).and_then(|x| x.get(j)) } else { None },
        heightmap.get(i+1).and_then(|x| x.get(j)),
        if j > 0 { heightmap.get(i).and_then(|x| x.get(j-1)) } else { None },
        heightmap.get(i).and_then(|x| x.get(j+1))
    ]
    .into_iter()
    .filter_map(|p| {
        match p {
            None => None,
            Some(v) => Some(*v)
        }
    })
    .collect()
}
