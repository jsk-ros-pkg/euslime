import json
import logging
import logging.config


DEFAULT_CONFIG = {
    'version': 1,
    'root': {
        'level': 'INFO',
        'handlers': ['console'],
    },
    'handlers': {
        'console': {
            'class': 'logging.StreamHandler',
            'level': 'INFO',
            'formatter': 'simple',
        },
    },
    'formatters': {
        'simple': {
            'format': '[%(levelname)s - %(module)s:L%(lineno)s] %(message)s',
        },
    },
}

_LOG_CONFIGURED = False

def get_logger(ns=__name__, cfg_path=None):
    global _LOG_CONFIGURED
    if _LOG_CONFIGURED is False:
        try:
            with open(cfg_path, "r") as f:
                cfg = json.load(f, encoding='utf-8', parse_int=True, parse_float=True)
                logging.config.dictConfig(cfg)
        except:
            logging.config.dictConfig(DEFAULT_CONFIG)
        _LOG_CONFIGURED = True
    return logging.getLogger(ns)
