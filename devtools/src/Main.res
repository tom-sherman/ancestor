module Modules = {
  type t = Styles | Base | Box | Grid

  let libPath = NodeJs.Path.join([NodeJs.Global.dirname, "..", "..", "lib", "src"])
  let responsivePropsPath = Node.Path.join([libPath, "core", "Ancestor_Styles.res"])
  let getPath = v =>
    switch v {
    | Styles => responsivePropsPath
    | _ => ``
    }
}

module Props = {
  type kind = Common | Responsive | Style
  type prefix = Forward | Declaration | DeclarationWithType

  let prefixToString = v =>
    switch v {
    | Forward => `forward-`
    | DeclarationWithType
    | Declaration => `declare-`
    }

  let getPropName = (~prefix, line) => {
    let regex = switch prefix {
    | Declaration => %re("/~(.*?)=\?/")
    | DeclarationWithType => %re("/~(.*?):/")
    | Forward => %re("/\?(.*?)/")
    }

    line->Js.String2.match_(regex)->Belt.Option.flatMap(Belt.Array.get(_, 1))
  }

  let kindToString = v =>
    switch v {
    | Common => `common-props`
    | Responsive => `responsive-props`
    | Style => `style-props`
    }

  let group = (~kind, ~prefix, ~position) =>
    `${prefix->prefixToString}${kind->kindToString}:${position}`

  let getProps = (~kind, ~prefix, moduleName: Modules.t) => {
    let start = group(~kind, ~prefix, ~position=`start`)
    let end = group(~kind, ~prefix, ~position=`end`)
    let modulePath = moduleName->Modules.getPath

    let fileContent = NodeJs.Fs.readFileSync(modulePath)->Node.Buffer.toString
    let lines = fileContent->Js.String2.split("\n")
    let startIndex = lines->Belt.Array.getIndexBy(Js.Re.fromString(start)->Js.Re.test_)
    let endIndex = lines->Belt.Array.getIndexBy(Js.Re.fromString(end)->Js.Re.test_)

    switch (startIndex, endIndex) {
    | (Some(start), Some(end)) =>
      Js.log2(start, end)
      lines
      ->Belt.Array.keepWithIndex((_, index) => index > start && index < end)
      ->Belt.Array.map(getPropName(~prefix))
      ->Js.log
    | _ => ()
    }
  }
}

Props.getProps(~kind=Style, ~prefix=DeclarationWithType, Styles)
