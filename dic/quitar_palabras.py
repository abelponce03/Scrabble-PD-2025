def filtrar_palabras_cortas(archivo_entrada, archivo_salida='diccionario_filtrado.txt'):
    """
    Elimina palabras de una sola letra de un archivo de diccionario.
    
    Args:
        archivo_entrada (str): Ruta del archivo de diccionario original
        archivo_salida (str): Ruta del archivo de salida (opcional)
    """
    try:
        with open(archivo_entrada, 'r', encoding='utf-8') as f_in:
            palabras = [linea.strip() for linea in f_in if len(linea.strip()) > 1]
            
        with open(archivo_salida, 'w', encoding='utf-8') as f_out:
            f_out.write('\n'.join(palabras))
            
        print(f"Archivo filtrado creado: {archivo_salida}")
        print(f"Palabras eliminadas: {len(palabras)} palabras conservadas")
        
    except FileNotFoundError:
        print(f"Error: No se encontró el archivo {archivo_entrada}")
    except Exception as e:
        print(f"Error inesperado: {str(e)}")

# Ejemplo de uso
filtrar_palabras_cortas('es.txt')