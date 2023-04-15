use std::error::Error;
use hecs_schedule::GenericWorld;

use hecs::{Entity, World};
use hecs_hierarchy::knot::*;

fn main() -> Result<(), Box<dyn Error>> {
    // Marker type which allows several hierarchies.
    struct Tree;

    let mut world: World = hecs_hierarchy::knot::HierarchyDag::default();

    //// Create a root entity, there can be several.
    //let root = world.spawn(("Root",));

    //// Create a loose entity
    //let child = world.spawn(("Child 1",));

    //// Attaches the child to a parent, in this case `root`
    //world.attach::<Tree>(child, root).unwrap();

    //// Iterate children
    //for child in world.children::<Tree>(root) {
    //    let name = world.get::<&str>(child).unwrap();
    //    println!("Child: {:?} {}", child, *name);
    //}

    //// Add a grandchild
    //world.attach_new::<Tree, _>(child, ("Grandchild",)).unwrap();

    //// Iterate recursively
    //for child in world.descendants_depth_first::<Tree>(root) {
    //    let name = world.get::<&str>(child).unwrap();
    //    println!("Child: {:?} {}", child, *name)
    //}

    //// Detach `child` and `grandchild`

    //let child2 = world.attach_new::<Tree, _>(root, ("Child 2",)).unwrap();

    //// Reattach as a child of `child2`
    //world.attach::<Tree>(child, child2).unwrap();

    //world.attach_new::<Tree, _>(root, ("Child 3",)).unwrap();

    //// Hierarchy now looks like this:
    //// Root
    //// |-------- Child 3
    //// |-------- Child 2
    ////           |-------- Child 1
    ////                     |-------- Grandchild

    //print_tree::<Tree>(&world, root);

    //world.despawn_all::<Tree>(child2);

    //print_tree::<Tree>(&world, root);

    //world
    //    .iter()
    //    .for_each(|entity| println!("Entity: {:?}", entity.entity()));

    let root = world.spawn(("Root",));
    let child1 = world.attach_new::<Tree, _>(root, ("Child1",)).unwrap();
    let child2 = world.attach_new::<Tree, _>(root, ("Child2",)).unwrap();
    let _child3 = world.attach_new::<Tree, _>(child2, ("Child3",)).unwrap();
    let child4 = world.attach_new::<Tree, _>(root, ("Child4",)).unwrap();
    let child5 = world.attach_new::<Tree, _>(root, ("Child5",)).unwrap();
    let child5b = world.attach_new::<Tree, _>(child5, ("Child5b",)).unwrap();
    //let child6 = world.attach_new::<Tree, _>(child2, ("Child6",)).unwrap();
    let child6 = world.knot::<Tree, _>(child2, child5b, ("Child6",)).unwrap();
    let child7 = world.attach_new::<Tree, _>(child6, ("Child7",)).unwrap();
    let child8 = world.attach_new::<Tree, _>(_child3, ("Child8",)).unwrap();

    print_tree::<Tree, _>(&world, root);

    // Remove child2, and by extension child3
    //world.detach::<Tree>(child2).unwrap();

    // Reattach child2 and child3 under child4
    //world.attach::<Tree>(child2, child4).unwrap();

    let order = [child1, child2, child4, child5];

    assert_eq!(
        world.children::<Tree>(root).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    let order = [_child3, child6];

    assert_eq!(
        world.children::<Tree>(child2).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    let order = [child6];

    assert_eq!(
        world.children::<Tree>(child5b).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    let order = [_child3, child8, child6, child7];

    assert_eq!(
        world.descendants_depth_first::<Tree>(child2).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    let order = [_child3, child6, child8, child7];

    assert_eq!(
        world.descendants_breadth_first::<Tree>(child2).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    let order = [child6, child2, root, child5b, child5];

    assert_eq!(
        world.ancestors::<Tree>(child7).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    world.detach::<Tree>(child6, DetachMode::IGNORE)?;

    let order = [_child3, child8];

    assert_eq!(
        world.descendants_depth_first::<Tree>(child2).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    world.attach::<Tree>(child6, child2)?;

    let order = [_child3, child8, child6, child7];

    assert_eq!(
        world.descendants_depth_first::<Tree>(child2).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    world.detach::<Tree>(child7, DetachMode::PRUNE)?;

    let order = [_child3, child8, child6];

    assert_eq!(
        world.descendants_depth_first::<Tree>(child2).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    world.attach::<Tree>(child7, child6)?;

    let order = [_child3, child8, child6, child7];

    assert_eq!(
        world.descendants_depth_first::<Tree>(child2).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    print_tree::<Tree, _>(&world, root);

    world.detach::<Tree>(child2, DetachMode::PRUNE)?;

    let order = [child1, child4, child5, child5b];

    assert_eq!(
        world.descendants_depth_first::<Tree>(root).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    world.attach::<Tree>(child2, child5b)?;

    let order = [child1, child4, child5, child5b, child2, _child3, child8, child6, child7];

    assert_eq!(
        world.descendants_depth_first::<Tree>(root).collect::<Vec<_>>(),
        order.iter().cloned().collect::<Vec<_>>()
    );

    print_tree::<Tree, _>(&world, root);

    Ok(())
}

fn print_tree<T: 'static + Send + Sync, W: GenericWorld + HierarchyDag>(world: &W, root: Entity) {
    let mut knots_seen: Vec<u32> = vec![];
    fn internal<T: 'static + Send + Sync, W: GenericWorld + HierarchyDag>(world: &W, parent: Entity, depth: usize, knots_seen: &mut Vec<u32>) {
        for child in HierarchyDag::children::<T>(world, parent) {
            let name = world.try_get::<&str>(child).unwrap();
            let is_knot = world.try_get::<Knot<T>>(child).is_ok();

            let is_seen = if is_knot {
                if knots_seen.contains(&child.id()) {
                    true
                } else {
                    knots_seen.push(child.id());
                    false
                }
            } else {
                false
            };

            println!(
                "{}|-------- {} {}",
                std::iter::repeat(" ")
                    .take((depth - 1) * 10)
                    .collect::<String>(),
                *name,
                if is_knot { if is_seen { "(^)" } else { "(&)" } } else { "" }
            );

            if !is_seen {
                internal::<T, W>(world, child, depth + 1, knots_seen)
            }
        }
    }

    let name = world.try_get::<&str>(root).unwrap();
    println!("{}", *name);
    internal::<T, W>(world, root, 1, &mut knots_seen)
}
