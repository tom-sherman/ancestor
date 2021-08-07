open Ancestor.Default

let s = React.string

@module("@emotion/css") external css: string => string = "css"

let box = css(`
  background: rgba(0,0,0,0.1);
  border: solid 2px #000;
  border-radius: 6px;
  padding: 32px;
  color: #000;
  font-family: 'DM Sans';
  font-size: 18px;
  display: flex;
  align-items: center;
  justify-content: center;
`)

@react.component
let make = () => {
  let {path} = RescriptReactRouter.useUrl()

  switch path {
  | list{} =>
    <div>
      <Grid>
        <Box columns=[xxs(#12), md(#4)]> <div className=box> {"4 columns"->s} </div> </Box>
        <Box columns=[xxs(#6), md(#4)]> <div className=box> {"4 columns"->s} </div> </Box>
        <Box columns=[xxs(#6), md(#4)]> <div className=box> {"4 columns"->s} </div> </Box>
      </Grid>
    </div>
  | list{"custom"} => <CustomConfig />
  | _ => "Not found"->s
  }
}
