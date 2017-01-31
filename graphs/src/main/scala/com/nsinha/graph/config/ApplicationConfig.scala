package com.nsinha.graph.config

import com.typesafe.config.{Config, ConfigFactory}


/**
  * Created by nsinha on 1/30/17.
  */
object ApplicationConfig {

  val conf: Config = ConfigFactory.load("application");
}
