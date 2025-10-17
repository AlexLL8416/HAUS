1. Sistema de Detección de Colisiones


Revisar el Separating Axis Theorem (SAT): Estudia el teorema del eje separador en: https://programmerart.weebly.com/separating-axis-theorem.html


Implementar las siguientes funciones de colisión:


* checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.
* detectRobotProjectileCollisions: Verifica qué proyectiles han colisionado con algún agente. Cuando detecte una colisión, debe generar el evento de colisión correspondiente.
* detectRobotRobotCollisions: Comprueba y detecta las colisiones entre los diferentes robots del juego. Deberá generar el evento de colisión correspondiente.
* checkCollisions: Función principal que coordina todas las comprobaciones de colisión.


2. Organización del Código en Módulos


Revisar el tema de Módulos: Estudia cómo estructurar el código en módulos para mejorar la organización, mantenibilidad y reutilización del código.


Reorganizar el código implementado: Divide tu implementación actual en módulos lógicos (por ejemplo: módulo de física, módulo de entidades, módulo de colisiones, módulo de renderizado, módulo de IA/comportamiento, etc.).


3. Abstracción mediante Tipos Genéricos


Crear tipo genérico base: Diseña un tipo de datos genérico que abstraiga las propiedades comunes de todos los objetos del juego (posición, velocidad, tamaño, etc.).


Reimplementar tipos existentes: Refactoriza tus tipos de datos actuales para que hereden o utilicen este tipo genérico base.


Actualizar funciones: Modifica las funciones implementadas previamente para que trabajen con el nuevo sistema de tipos genéricos, asegurando compatibilidad y consistencia.



4. Sistema de Memoria para Agentes


Los agentes necesitan una memoria (implementada como diccionario) para tomar decisiones inteligentes. Crea un tipo de datos flexible que pueda almacenar diferentes tipos de información: enteros, cadenas de texto, puntos/coordenadas, booleanos, etc.



5. DSL para acciones del Bot


Crea tipo para definir las acciones que se pueden indicar sobre un Bot.


Implementa un bot de ejemplo: Implementa una función que recibe la información sobre el estado del juego y devuelve un conjunto de acciones definidas por el tipo anterior.