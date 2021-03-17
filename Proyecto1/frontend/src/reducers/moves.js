import { SET_MOVE, VERIFY_MOVE, RESET, SOLVING } from '../constants/actionTypes'
import { sumCoordinates, isValidMove, scalarMult } from './_utils'

function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min)) + min;
}

let opciones = [[1,4,2,3,7,5,6,8,0],
                [1,4,0,6,3,2,7,8,5],
                [7,6,8,0,2,3,4,5,1],
                [7,2,0,4,1,6,3,8,5],
                [4,2,7,6,1,3,0,8,5],
                [0,5,4,1,3,7,6,8,2],
                [5,4,1,0,8,7,3,6,2],
                [1,2,3,4,5,6,7,0,8],
                [6,7,2,0,1,5,3,8,4],
                [8,3,2,6,0,4,7,1,5],
                [2,3,5,6,1,7,0,8,4],
                [2,0,5,8,3,6,7,1,4],
                [0,5,4,1,6,2,7,3,8],
                [5,7,6,1,0,2,4,3,8],
                [3,7,0,4,8,2,1,5,6]]

let r = getRandomInt(0,opciones.length)
const gridData = opciones[r]

const grid = gridData.map((digit, i) => {
  return {
    digit,
    pos: {
      x: Math.trunc(i / 3),
      y: Math.trunc(i % 3)
    },
    delta: {
      x: 0,
      y: 0
    }
  }
})
/*const getBlankPosition = grid => {
  const { pos } = grid.filter(d => d.digit === 0)[0]
  console.log(pos)
  return {"x":2,"y":0}
}*/
const getBlankPosition = grid => {
  let index = gridData.indexOf(0)
  let y = Math.trunc(index/3)
  let x = index%3
  console.log(x)
  console.log(y)
  return {"x":x,"y":y}
}

const initialState = {
  grid,
  gridData,
  blankPosition: getBlankPosition(grid),
  targetPosition: { x: 0, y: 0 },
  targetDigit: 0,
  canAnimate: false,
  isSolving: false,
  shift: { x: 0, y: 0 }
}

const resolveSetMove = state => {
  const oldGrid = state.grid
  const oldGridData = state.gridData

  const { targetDigit, shift, targetPosition, blankPosition } = state

  const resolvePositionDelta = ({ digit, delta }) =>
    digit === 0
      ? { pos: targetPosition, delta: sumCoordinates(delta, shift) }
      : digit === targetDigit
        ? {
          pos: blankPosition,
          delta: sumCoordinates(delta, scalarMult(-1, shift))
        }
        : {}

  const grid = oldGrid.map(attr => {
    return { ...attr, ...resolvePositionDelta(attr) }
  })

  const gridData = oldGridData.map(d => {
    return d === 0 ? targetDigit : d === targetDigit ? 0 : d
  })
  //console.log(gridData)
  return {
    grid,
    gridData,
    blankPosition: targetPosition,
    shift: { x: 0, y: 0 },
    canAnimate: false
  }
}

const verifyMove = ({ blankPosition, gridData }, shift) => {
  const nextPosition = sumCoordinates(blankPosition, shift)

  const [targetPosition, canAnimate] = isValidMove(nextPosition)
    ? [nextPosition, true]
    : [blankPosition, false]
  const targetDigit = gridData[targetPosition.y * 3 + targetPosition.x]

  return {
    targetPosition,
    targetDigit,
    shift,
    canAnimate
  }
}

export default function moves (state = initialState, action) {
  switch (action.type) {
    case VERIFY_MOVE:
      return { ...state, ...verifyMove(state, action.shift) }
    case SET_MOVE:
      return { ...state, ...resolveSetMove(state) }
    case RESET:
      return { ...initialState }
    case SOLVING:
      return { ...state, isSolving: action.bool }
    default:
      return state
  }
}