package data

trait XStreamable {
  data.XStream.alias(alias(), this.getClass);
  def alias(): String = this.getClass.getSimpleName().toLowerCase()
}