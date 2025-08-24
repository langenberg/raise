from openai import OpenAI

class Gpt:
    def __init__(self):
        self.client = None
        self.api_key = None
        self.assistant_list = AssistantList()

    def check_client_ready(self):
        if self.client is None:
            Exception("Client not ready. Re-run power_up(api_key) to fix the issue.")
        
    def power_up(self, api_key = None, reuse = True):
        self.api_key = api_key
        self.client = OpenAI(api_key = api_key)
        self.create_assistants(reuse)
        return True
    
    def get_assistant_id(self, assistant_name):
        assistant_id = self.assistant_list.get_id(assistant_name = assistant_name)
        if assistant_id is None:
            self.create_assistants(reuse)
        assistant_id = self.assistant_list.get_id(assistant_name = assistant_name)
        return assistant_id
    
    def get_assistant_by_name(self, assistant_name):
        assistant_id = self.assistant_list.get_id(assistant_name = assistant_name)
        if assistant_id is None:
            self.create_assistants(reuse)
        return self.assistant_list.find_assistant_by_name(assistant_name = assistant_name)
    
    # def reconnect(self, api_key = self.api_key, assistant_id = self.assistant_id):
    #     self.power_up(api_key = api_key, assistant_id = assistant_id)
    #     return True
    # 
    # def set_api_key(self, api_key):
    #     self.reconnect(api_key = api_key)
    #     return True
        
    def shut_down(self):
        pass
        
    def create_assistants(self, reuse = True):
        existing_assistants = self.client.beta.assistants.list().data
        for assistant in self.assistant_list.assistants:
            results = [existing_assistant for existing_assistant in existing_assistants if existing_assistant.name == assistant.name]
            if len(results) > 0 and reuse:
                new_assistant = results[0]
            else:
                new_assistant = self.client.beta.assistants.create(
                    name=assistant.name,
                    instructions=assistant.instructions,
                    model=assistant.model,
                    tools=assistant.tools,
                    response_format=assistant.response_format
                )
            assistant.id = new_assistant.id
        return [assistant.id for assistant in self.assistant_list.assistants]

    def delete_assistant(self, assistant_id):
        assistants = self.client.beta.assistants.list().data
        for assistant in assistants:
            if assistant.id == assistant_id:
                self.client.beta.assistants.delete(assistant.id)
        return assistant_id
    
    def create_thread(self):
        thread = self.client.beta.threads.create()
        return thread.id
    
    def delete_thread(self, thread_id, delete_messages = True):
        if delete_messages:
            for message in self.client.beta.threads.messages.list(thread_id).data:
                self.client.beta.threads.messages.delete(
                    message_id = message.id,
                    thread_id = thread_id
                )
        thread = self.client.beta.threads.delete(thread_id)
        return thread_id
    
    def flush_assistants(self):
        self.check_client_ready()
        existing_assistants = self.client.beta.assistants.list().data
        assistant_names = [assistant.name for assistant in self.assistant_list.assistants]
        deleted_assistants = []
        for existing_assistant in existing_assistants:
            if any(existing_assistant.name == assistant_name for assistant_name in assistant_names):
                self.client.beta.assistants.delete(existing_assistant.id)
                deleted_assistants.append(existing_assistant.id)
        return deleted_assistants
        
    def create_vector_store(self, thread_id = None):
        self.check_client_ready()
        vector_store = self.client.vector_stores.create(
            name="slides",
            expires_after={"anchor": "last_active_at", "days": 1}
        )
        if not thread_id is None:
            self.add_vector_stores_to_thread(
                vector_store_ids = [vector_store.id],
                thread_id = thread_id
            )
        return vector_store.id
    
    def add_vector_stores_to_thread(self, vector_store_ids, thread_id):
        thread = self.client.beta.threads.update(
            thread_id,
            tool_resources={"file_search": {"vector_store_ids": vector_store_ids}}
        )
        return(thread_id)
        
    def delete_vector_store(self, vector_store_id, delete_files = True):
        self.check_client_ready()
        for vector_store in self.client.vector_stores.list().data:
            if (vector_store.id == vector_store_id):
                if delete_files:
                    for file in self.client.vector_stores.files.list(vector_store_id = vector_store_id).data:
                        self.delete_file(file.id)
                self.client.vector_stores.delete(vector_store.id)
        return vector_store_id

    def list_vector_stores(self, names_only = False):
        self.check_client_ready()
        vector_stores = self.client.vector_stores.list().data
        vector_store_names = [vector_store.name for vector_store in vector_stores]
        if names_only:
            return vector_store_names
        else:
            return vector_stores

    def flush_vector_stores(self, delete_files = True):
        self.check_client_ready()
        vector_store_ids = [vector_store.id for vector_store in self.client.vector_stores.list().data]
        for vector_store_id in vector_store_ids:
            self.delete_vector_store(vector_store_id, delete_files)
        return vector_store_ids
    
    def create_message(self, thread_id, role, content):
        message = self.client.beta.threads.messages.create(
            thread_id,
            role = role,
            content = content
        )
        return message.id
    
    def delete_message(self, thread_id, message_id):
        message = self.client.beta.threads.messages.delete(
            thread_id = thread_id,
            message_id = message_id
        )
        return message.id
    
    def list_messages(self, thread_id):
        messages = self.client.beta.threads.messages.list(thread_id).data
        return [
            {
                "id": message.id, 
                "role": message.role, 
                "content": message.content[0].text.value
            } 
            for message in messages
        ]
    
    def list_files(self, names_only = False):
        self.check_client_ready()
        files = self.client.files.list().data
        filenames = [file.filename for file in files]
        if names_only:
            return filenames
        else:
            return files
    
    def delete_file(self, file_id):
        self.check_client_ready()
        self.client.files.delete(file_id)
        return file_id
    
    def delete_files(self, file_ids):
        self.check_client_ready()
        for file_id in file_ids:
            self.client.files.delete(file_id)
        return file_ids
            
    def flush_files(self):
        self.check_client_ready()
        files = self.client.files.list().data
        file_ids = [file.id for file in files]
        for file in files:
            self.client.files.delete(file.id)
        return file_ids
        
    def upload_files(self, file_paths):
        self.check_client_ready()
        
        message_files = [self.client.files.create(file = open(path, "rb"), purpose = "assistant") for path in file_paths]
        
        return [message_file.id for message_file in message_files]

    def upload_files_to_vector_store(self, file_paths, vector_store_id = None):
        file_streams = [open(path, "rb") for path in file_paths]
        
        if vector_store_id is None:
            vector_store_id = self.create_vector_store()
        
        file_batch = self.client.vector_stores.file_batches.upload_and_poll(
            vector_store_id=vector_store_id, files=file_streams
        )
        
        vector_store_files = self.client.vector_stores.file_batches.list_files(
            vector_store_id=vector_store_id,
            batch_id=file_batch.id
        )
        
        file_ids = [file.id for file in vector_store_files.data]
        
        return file_ids
    
    def delete_files_from_vector_store(self, file_ids, vector_store_id):
        for file_id in file_ids:
            deleted_vector_store_file = self.client.vector_stores.files.delete(
                vector_store_id=vector_store_id,
                file_id=file_id
            )
        return file_ids
    
    def completion_request(self, messages):
        self.check_client_ready()
        
        assistant = self.get_assistant_by_name("assistant_json")
        
        messages_new = [{"role": "system", "content": "You are a helpful assistant designed to output JSON."}]
        messages_new.extend(messages)
        
        completion = self.client.chat.completions.create(
            model = assistant.model,
            response_format = {"type": "json_object"},
            messages = messages_new
        )
        return completion.choices[0].message.content
    
    def verify_json(self, json):
        self.check_client_ready()
        content = "Please check the following JSON string correct for correctness. Please correct the syntax to make it a valid JSON string. If there is additional text before or after the string, drop it. Only return the new JSON string.\n\n## Start of the JSON string:\n\n"
        content = content + json
        messages = [
            {"role": "user", "content": content}
        ]
        
        return self.completion_request(messages)
    
    def assistant_request(self, thread_id, assistant_name = "assistant_json", verify_json = True, drop_message = False):
        self.check_client_ready()
        
        assistant_id = self.get_assistant_id(assistant_name)
        
        run = self.client.beta.threads.runs.create_and_poll(
            thread_id = thread_id, assistant_id = assistant_id
        )
        
        messages = list(self.client.beta.threads.messages.list(thread_id=thread_id, run_id=run.id))
        
        message_content = messages[0].content[0].text
        annotations = message_content.annotations
        citations = []
        for index, annotation in enumerate(annotations):
            message_content.value = message_content.value.replace(annotation.text, f"[{index}]")
            if file_citation := getattr(annotation, "file_citation", None):
                cited_file = self.client.files.retrieve(file_citation.file_id)
                citations.append(f"[{index}] {cited_file.filename}")
        
        if drop_message:
            for message in messages:
                self.delete_message(thread_id = thread_id, message_id = message.id)
        
        if verify_json:
            return self.verify_json(message_content.value)
        else:
            return message_content.value
    
