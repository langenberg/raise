
class Assistant:
    def __init__(self, name, instructions, model, response_format = None, tools = None, assistant_id = None):
        self.name = name
        self.instructions = instructions
        self.model = model
        self.response_format = response_format
        self.tools = tools
        self.id = assistant_id

class AssistantList:
    def find_assistant_by_name(self, assistant_name):
        assistants = [assistant for assistant in self.assistants if assistant.name == assistant_name]
        if len(assistants) == 0:
            return None
        else:
            return assistants[0]
    
    def set_id(self, assistant_name, assistant_id):
        assistant = self.find_assistant_by_name(assistant_name)
        if not assistant is None:
            assistant.id = assistant_id
            
    def get_id(self, assistant_name):
        assistant = self.find_assistant_by_name(assistant_name)
        if not assistant is None:
            return assistant.id
    
    def has_id(self, assistant_name):
        return not self.get_id(assistant_name) is None
    
    def __init__(self):
        self.assistants = [
            Assistant(
                name = "assistant_chat",
                instructions = "You are a helpful assistant.",
                model = "gpt-4o-2024-08-06",
                response_format = None,
                tools = [
                    {
                        "type": "file_search",
                        "file_search": {
                            "max_num_results": 10
                        }
                    },
                    {"type": "code_interpreter"}
                ],
            ),
            Assistant(
                name = "assistant_json",
                instructions = "You are a helpful assistant designed to output JSON.",
                model = "gpt-4o-2024-08-06",
                response_format = None,
                tools = [
                    {
                        "type": "file_search",
                        "file_search": {
                            "max_num_results": 10
                        }
                    },
                    {"type": "code_interpreter"}
                ],
            ),
            # Assistant(
            #     name = "assistant_description",
            #     instructions = "You are a helpful assistant designed to output JSON.",
            #     model = "gpt-4o-2024-08-06",
            #     tools = None,
            #     response_format = {
            #         "type": "json_schema",
            #         "json_schema": {
            #             "name": "json_schema_description",
            #             "schema": {
            #                 "$schema": "http://json-schema.org/draft-07/schema#",
            #                 "type": "object",
            #                 "properties": {
            #                     "study_detail": {
            #                         "type": "string",
            #                         "description": "A brief description of the study and its objective."
            #                     },
            #                     "variables": {
            #                         "type": "object",
            #                         "properties": {
            #                             "outcome_variable": {
            #                                 "type": "object",
            #                                 "properties": {
            #                                     "name": {
            #                                         "type": "string",
            #                                         "description": "The name of the outcome variable."
            #                                     },
            #                                     "scale": {
            #                                         "type": "string",
            #                                         "enum": ["continuous", "binary", "categorical"],
            #                                         "description": "The type of scale used for the outcome variable."
            #                                     },
            #                                     "detail": {
            #                                         "type": "string",
            #                                         "description": "A detailed description of the outcome variable."
            #                                     }
            #                                 },
            #                                 "required": ["name", "scale", "detail"]
            #                             },
            #                             "independent_variables": {
            #                                 "type": "object",
            #                                 "properties": {
            #                                     "continuous_variables": {
            #                                         "type": "array",
            #                                         "items": {
            #                                             "type": "object",
            #                                             "properties": {
            #                                                 "name": {
            #                                                     "type": "string",
            #                                                     "description": "The name of the continuous variable."
            #                                                 },
            #                                                 "scale": {
            #                                                     "type": "string",
            #                                                     "enum": ["continuous"],
            #                                                     "description": "The scale type of the continuous variable."
            #                                                 },
            #                                                 "distribution": {
            #                                                     "type": "object",
            #                                                     "properties": {
            #                                                         "mean": {
            #                                                             "type": "number",
            #                                                             "description": "The mean of the distribution."
            #                                                         },
            #                                                         "standard_deviation": {
            #                                                             "type": "number",
            #                                                             "description": "The standard deviation of the distribution."
            #                                                         }
            #                                                     },
            #                                                     "required": ["mean", "standard_deviation"],
            #                                                     "description": "The distribution of the continuous variable."
            #                                                 },
            #                                                 "detail": {
            #                                                     "type": "string",
            #                                                     "description": "A detailed description of the continuous variable."
            #                                                 }
            #                                             },
            #                                             "required": ["name", "scale", "distribution", "detail"]
            #                                         }
            #                                     },
            #                                     "binary_variables": {
            #                                         "type": "array",
            #                                         "items": {
            #                                             "type": "object",
            #                                             "properties": {
            #                                                 "name": {
            #                                                     "type": "string",
            #                                                     "description": "The name of the binary variable."
            #                                                 },
            #                                                 "scale": {
            #                                                     "type": "string",
            #                                                     "enum": ["binary"],
            #                                                     "description": "The scale type of the binary variable."
            #                                                 },
            #                                                 "categories": {
            #                                                     "type": "array",
            #                                                     "items": {
            #                                                         "type": "object",
            #                                                         "properties": {
            #                                                             "category": {
            #                                                                 "type": "string",
            #                                                                 "description": "The value of the category."
            #                                                             },
            #                                                             "label": {
            #                                                                 "type": "string",
            #                                                                 "description": "The label corresponding to the category."
            #                                                             }
            #                                                         },
            #                                                         "required": ["category", "label"]
            #                                                     },
            #                                                     "description": "Categories for the binary variable."
            #                                                 },
            #                                                 "reference": {
            #                                                     "type": "string",
            #                                                     "description": "The reference category for the binary variable."
            #                                                 },
            #                                                 "proportions": {
            #                                                     "type": "array",
            #                                                     "items": {
            #                                                         "type": "object",
            #                                                         "properties": {
            #                                                             "category": {
            #                                                                 "type": "string",
            #                                                                 "description": "The value of the category."
            #                                                             },
            #                                                             "proportion": {
            #                                                                 "type": "number",
            #                                                                 "description": "The proportion of the category."
            #                                                             }
            #                                                         },
            #                                                         "required": ["category", "proportion"]
            #                                                     },
            #                                                     "description": "The proportions of the binary variable categories."
            #                                                 },
            #                                                 "detail": {
            #                                                     "type": "string",
            #                                                     "description": "A detailed description of the binary variable."
            #                                                 }
            #                                             },
            #                                             "required": ["name", "scale", "categories", "reference", "proportions", "detail"]
            #                                         }
            #                                     },
            #                                     "categorical_variables": {
            #                                         "type": "array",
            #                                         "items": {
            #                                             "type": "object",
            #                                             "properties": {
            #                                                 "name": {
            #                                                     "type": "string",
            #                                                     "description": "The name of the categorical variable."
            #                                                 },
            #                                                 "scale": {
            #                                                     "type": "string",
            #                                                     "enum": ["categorical"],
            #                                                     "description": "The scale type of the categorical variable."
            #                                                 },
            #                                                 "categories": {
            #                                                     "type": "array",
            #                                                     "items": {
            #                                                         "type": "object",
            #                                                         "properties": {
            #                                                             "category": {
            #                                                                 "type": "string",
            #                                                                 "description": "The value of the category."
            #                                                             },
            #                                                             "label": {
            #                                                                 "type": "string",
            #                                                                 "description": "The label corresponding to the category."
            #                                                             }
            #                                                         },
            #                                                         "required": ["category", "label"]
            #                                                     },
            #                                                     "description": "Categories for the categorical variable."
            #                                                 },
            #                                                 "reference": {
            #                                                     "type": "string",
            #                                                     "description": "The reference category for the categorical variable."
            #                                                 },
            #                                                 "proportions": {
            #                                                     "type": "array",
            #                                                     "items": {
            #                                                         "type": "object",
            #                                                         "properties": {
            #                                                             "category": {
            #                                                                 "type": "string",
            #                                                                 "description": "The value of the category."
            #                                                             },
            #                                                             "proportion": {
            #                                                                 "type": "number",
            #                                                                 "description": "The proportion of the category."
            #                                                             }
            #                                                         },
            #                                                         "required": ["category", "proportion"]
            #                                                     },
            #                                                     "description": "The proportions of the categorical variable categories."
            #                                                 },
            #                                                 "detail": {
            #                                                     "type": "string",
            #                                                     "description": "A detailed description of the categorical variable."
            #                                                 }
            #                                             },
            #                                             "required": ["name", "scale", "categories", "reference", "proportions", "detail"]
            #                                         }
            #                                     }
            #                                 },
            #                                 "required": ["continuous_variables", "binary_variables", "categorical_variables"]
            #                             }
            #                         },
            #                         "required": ["outcome_variable", "independent_variables"]
            #                     }
            #                 },
            #                 "required": ["study_detail", "variables"]
            #             }
            #         }
            #     }
            # )
        ]

    
