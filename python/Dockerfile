FROM python:3.12-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY dcf/ dcf/
COPY dcf.py .
COPY config.json.example .

RUN python -m grpc_tools.protoc -I dcf/proto --python_out=dcf --grpc_python_out=dcf dcf/proto/messages.proto dcf/proto/services.proto

EXPOSE 50051

ENTRYPOINT ["python", "dcf.py"]
